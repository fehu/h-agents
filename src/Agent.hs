-----------------------------------------------------------------------------
--
-- Module      :  Agent
-- License     :  MIT
--
-- Maintainer  :  kdn.kovalev@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable (requires STM)
--
-----------------------------------------------------------------------------

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Agent(

  ReactiveAgent(..)
, Message, Response, ExpectedResponse, MessageResponse
, handleResponseAsync, handleResponseSuccessAsync
, waitResponse, waitResponseSuccess

, AgentControl(..), AgentInnerInterface(..)
, AgentResult(..), AgentExecutionResult
, EmptyResult(..)

, AgentRef, SomeAgentRef, someAgentRef

, CreateAgent(..), CreateAgentRef(..)

, GenericAgent
, GenericAgentDescriptor(..)
, MessageHandling(..), handlesNoMessages

, mbHandle, selectMessageHandler
, mbResp, selectResponse

) where

import Data.Typeable (Typeable, cast)
import Data.Maybe (fromMaybe, fromJust, isNothing)
import Data.Function (on)

import Control.Applicative ((<|>))
import Control.Monad (forever, when, unless)
import Control.Exception (Exception, SomeException(..), throwIO)

import Control.Concurrent (ThreadId, forkIO, forkFinally, killThread)
import Control.Concurrent.STM

--------------------------------------------------------------------------------

-- | Can receive and respond messages.
--   Priority mesages are handled before the normal ones.
class ReactiveAgent a where
  send :: (Message msg) => a -> msg -> IO ()
  ask  :: (MessageResponse msg resp) => a -> msg -> IO (Response resp)

  sendPriority :: (Message msg) => a -> msg -> IO ()
  askPriority  :: (MessageResponse msg resp) => a -> msg -> IO (Response resp)

--------------------------------------------------------------------------------

type Message msg = (Typeable msg, Show msg)
newtype Response resp = Response (TMVar (Maybe resp))

type family ExpectedResponse msg :: *

type MessageResponse msg resp = (Message msg, Message resp, ExpectedResponse msg ~ resp)

handleResponseAsync :: Response resp -> (Maybe resp -> IO ()) -> IO ()
handleResponseAsync resp f  = do
  thread <- forkIO $ waitResponse resp >>= f
  killThread thread

handleResponseSuccessAsync :: Response resp -> (resp -> IO ()) -> IO ()
handleResponseSuccessAsync resp f = handleResponseAsync resp f'
  where f' (Just r) = f r
        f' _        = fail "No response received"

waitResponse :: Response resp -> IO (Maybe resp)
waitResponse (Response respVar) = atomically $ takeTMVar respVar

waitResponseSuccess :: Response resp -> IO resp
waitResponseSuccess resp = do r <- waitResponse resp
                              case r of Just r' -> return r'
                                        _       -> fail "No response received"

--------------------------------------------------------------------------------

class AgentControl a
  where agentStart            :: a -> IO ()
        agentPause            :: a -> IO ()
        agentTerminate        :: a -> IO ()
        agentWaitTermination  :: a -> IO ()

        agentRunning    :: a -> IO Bool
        agentPaused     :: a -> IO Bool
        agentTerminated :: a -> IO Bool

        agentId       :: a -> String
        agentDebug    :: a -> IO Bool
        agentSetDebug :: a -> Bool -> IO ()
        printDebug    :: a -> String -> IO ()

        printDebug a str = do debug <- agentDebug a
                              when debug . putStrLn $
                                "[DEBUG][" ++ show (agentId a) ++ "] " ++ str

-----------------------------------------------------------------------------

type AgentExecutionResult res = Either SomeException res

class AgentResult a res | a -> res
  where
    agentResult     :: a -> IO (Maybe (AgentExecutionResult res))
    agentWaitResult :: a -> IO        (AgentExecutionResult res)

data AgentExecutionAborted = AgentExecutionAborted deriving (Typeable, Show)
instance Exception AgentExecutionAborted

--------------------------------------------------------------------------------

-- | Reference for 'ReactiveAgent' with 'AgentControl' and specific 'AgentResult'.
data AgentRef res = forall a . ( ReactiveAgent a
                               , AgentControl a
                               , AgentResult a res ) => AgentRef a

instance ReactiveAgent (AgentRef res) where
  send (AgentRef a) = send a
  ask  (AgentRef a) = ask a
  sendPriority (AgentRef a) = sendPriority a
  askPriority  (AgentRef a) = askPriority a

instance AgentControl (AgentRef res) where
  agentId              (AgentRef a) = agentId a
  agentDebug           (AgentRef a) = agentDebug a
  agentSetDebug        (AgentRef a) = agentSetDebug a
  agentStart           (AgentRef a) = agentStart a
  agentPause           (AgentRef a) = agentPause a
  agentTerminate       (AgentRef a) = agentTerminate a
  agentWaitTermination (AgentRef a) = agentWaitTermination a

  agentRunning         (AgentRef a) = agentRunning a
  agentPaused          (AgentRef a) = agentPaused a
  agentTerminated      (AgentRef a) = agentTerminated a

instance AgentResult (AgentRef res) res where
  agentResult     (AgentRef a) = agentResult a
  agentWaitResult (AgentRef a) = agentWaitResult a


instance Eq  (AgentRef res) where (==)    = (==)    `on` agentId
instance Ord (AgentRef res) where compare = compare `on` agentId

instance Show (AgentRef res) where show ref = "AgentRef " ++ show (agentId ref)

--------------------------------------------------------------------------------

-- | Reference for 'ReactiveAgent' with 'AgentControl' and some 'AgentResult'.
data SomeAgentRef = forall a res . ( ReactiveAgent a
                                   , AgentControl a
                                   , AgentResult a res ) => SomeAgentRef a

someAgentRef :: AgentRef res -> SomeAgentRef
someAgentRef (AgentRef a) = SomeAgentRef a

instance ReactiveAgent SomeAgentRef where
  send (SomeAgentRef a) = send a
  ask  (SomeAgentRef a) = ask a
  sendPriority (SomeAgentRef a) = sendPriority a
  askPriority  (SomeAgentRef a) = askPriority a

instance AgentControl SomeAgentRef where
  agentId              (SomeAgentRef a) = agentId a
  agentDebug           (SomeAgentRef a) = agentDebug a
  agentSetDebug        (SomeAgentRef a) = agentSetDebug a
  agentStart           (SomeAgentRef a) = agentStart a
  agentPause           (SomeAgentRef a) = agentPause a
  agentTerminate       (SomeAgentRef a) = agentTerminate a
  agentWaitTermination (SomeAgentRef a) = agentWaitTermination a

  agentRunning         (SomeAgentRef a) = agentRunning a
  agentPaused          (SomeAgentRef a) = agentPaused a
  agentTerminated      (SomeAgentRef a) = agentTerminated a

--------------------------------------------------------------------------------

-- | Agent is created from the corresponding descriptor.
class CreateAgent from res a | from -> res
  where
    createAgent :: from -> IO a

class ( CreateAgent from res (CreateAgentType from)
      , ReactiveAgent        (CreateAgentType from)
      , AgentControl         (CreateAgentType from)
      , AgentResult          (CreateAgentType from) res
      ) =>
  CreateAgentRef from res where
    type CreateAgentType from :: *
    createAgentRef :: from -> IO (AgentRef res)
    createAgentRef from = do (ag :: CreateAgentType from) <- createAgent from
                             return $ AgentRef ag

--------------------------------------------------------------------------------

class (AgentControl a, AgentResult a res) =>
  AgentInnerInterface a s res | a -> s, a -> res
    where
      agentRef      :: a -> AgentRef res
      agentState    :: a -> s
      agentResultSuccess :: a -> res -> IO ()
      agentResultFailure :: (Exception ex) => a -> ex -> IO ()


data AgentExecState = AgentRun | AgentPause | AgentTerminate
  deriving (Show, Eq)

data NormalTermination = NormalTermination deriving (Show, Typeable)
instance Exception NormalTermination

data MessageThreadException =
  forall ex . Exception ex => MessageThreadException ex
  deriving Typeable
instance Show MessageThreadException where
  show (MessageThreadException ex) = showMessageException ex
instance Exception MessageThreadException

data ActionThreadException =
  forall ex . Exception ex => ActionThreadException ex
  deriving Typeable
instance Show ActionThreadException where
  show (ActionThreadException ex) = showActionException ex
instance Exception ActionThreadException

showActionException  ex = "Exception in action thread: "  ++ show ex
showMessageException ex = "Exception in message thread: " ++ show ex

data AgentThreadsException =
  forall exM exA . (Exception exA, Exception exM) => AgentThreadsException exM exA
  deriving Typeable
instance Show AgentThreadsException where
  show (AgentThreadsException exA exM) = showActionException  exA ++ "\n" ++
                                         showMessageException exM
instance Exception AgentThreadsException

--------------------------------------------------------------------------------

data ReceivedMessage  = forall msg . Message msg =>
                               MessageNoResponse msg
                      | forall msg resp . MessageResponse msg resp =>
                               MessageAwaitsResponse msg (Maybe resp -> IO ())

-- | Active and Reactive agent implementation.
--   Has internal state 's'.
--   Message handling and action run in separate threads.
data GenericAgent s res = GenericAgent {
    _name               :: String
  , _debug              :: TVar Bool
  , _state              :: s
  , _execState          :: TVar AgentExecState
  , _result             :: TMVar (AgentExecutionResult res)
  , _messageBox         :: TQueue ReceivedMessage
  , _messagePriorityBox :: TQueue ReceivedMessage
  , _messageThread      :: TVar AgentThread
  , _actionThread       :: TVar AgentThread
  }

data AgentThread = AgentThread ThreadId (TMVar (Maybe SomeException))

newAgentThread :: IO () -> IO AgentThread
newAgentThread f = do  threadTerminated  <- newEmptyTMVarIO
                       threadId <- forkFinally f $ atomically
                                                 . putTMVar threadTerminated
                                                 . either terminationEx (const Nothing)
                       return $ AgentThread threadId threadTerminated
  where terminationEx e@(SomeException ex) =
          case cast ex of Just NormalTermination -> Nothing
                          _                      -> Just e

agentThreadTerminated (AgentThread _ terminated) = fmap not . atomically
                                                 $ isEmptyTMVar terminated

-- | Waits for any of two thread to terminate. Then sets 'AgentTerminate'
-- execution state (if not set already) and waits termination of another thread.
-- Wraps exceptions in 'MessageThreadException', 'ActionThreadException' or
-- 'AgentThreadsException'.
awaitAgentThreads a = do
    msgThread <- readTVar $ _messageThread a
    actThread <- readTVar $ _actionThread a
    let waitAgentThread (AgentThread _ terminated) = readTMVar terminated
        waitMsgThread = waitAgentThread msgThread
        waitActThread = waitAgentThread actThread
    -- wait any thread termination
    waitMsgThread <|> waitActThread
    exState <- readTVar $ _execState a
    -- set 'AgentTerminate' execution state, if not already set
    unless (exState == AgentTerminate) $ _execState a `writeTVar` AgentTerminate
    -- wait both threads to terminate
    termination <- (,) <$> waitActThread
                       <*> waitMsgThread
    return $ case termination
              of (Just (SomeException exA), Just (SomeException exM)) ->
                        Just . SomeException $ AgentThreadsException  exA exM
                 (Just (SomeException exA), Nothing)                  ->
                        Just . SomeException $ ActionThreadException  exA
                 (Nothing,                  Just (SomeException exM)) ->
                        Just . SomeException $ MessageThreadException exM
                 (Nothing,                  Nothing)                  -> Nothing

putMessageInBox box msg = atomically $ box `writeTQueue` MessageNoResponse msg
putMessageRespInBox box msg = do
  respBox <- newEmptyTMVarIO
  let  respond = atomically . putTMVar respBox
  atomically $ box `writeTQueue` MessageAwaitsResponse msg respond
  return $ Response respBox

setExecState exState a = atomically $ _execState a `writeTVar` exState

data MessageHandling s res = MessageHandling{
    msgHandle   :: forall c msg . (AgentInnerInterface c s res, Message msg) =>
                    c -> msg -> Maybe (IO ())
  , msgRespond  :: forall c msg resp . ( AgentInnerInterface c s res
                                       , MessageResponse msg resp) =>
                    c -> msg -> Maybe (IO resp)
  }

handlesNoMessages = MessageHandling (selectMessageHandler [])
                                    (selectResponse [])

instance ReactiveAgent (GenericAgent s res) where
  send a = putMessageInBox (_messageBox a)
  ask a  = putMessageRespInBox (_messageBox a)

  sendPriority a = putMessageInBox (_messagePriorityBox a)
  askPriority a  = putMessageRespInBox (_messagePriorityBox a)

instance AgentControl (GenericAgent s res) where
  agentId         = _name
  agentDebug      = readTVarIO . _debug
  agentSetDebug a = atomically . writeTVar (_debug a)
  agentStart      = setExecState AgentRun
  agentPause      = setExecState AgentPause
  agentTerminate  = setExecState AgentTerminate

  agentRunning    = isExecState AgentRun
  agentPaused     = isExecState AgentPause
  agentTerminated a = do msgThread <- readTVarIO $ _messageThread a
                         actThread <- readTVarIO $_actionThread a
                         (&&) <$> agentThreadTerminated msgThread
                              <*> agentThreadTerminated actThread

  agentWaitTermination a = do a `printDebug` "agentWaitTermination"
                              mbException <- atomically $ awaitAgentThreads a
                              printDebug a $ "Agent terminated " ++
                                case mbException
                                  of Just ex -> "with error:\n" ++ show ex
                                     _       -> "normally."

isExecState state = fmap (== state) . readTVarIO . _execState
abortedExceptionResult = Left $ SomeException AgentExecutionAborted

instance AgentResult (GenericAgent s res) res where
  agentResult a = do mbResult   <- atomically . tryTakeTMVar $ _result a
                     terminated <- agentTerminated a
                     return $ if terminated && isNothing mbResult
                               then Just abortedExceptionResult
                               else mbResult

  agentWaitResult a = do
    wait <- atomically $  Right <$> takeTMVar (_result a)
                      <|> Left  <$> awaitAgentThreads a
    return $ either (Left . fromMaybe (SomeException AgentExecutionAborted))
                    id wait

instance AgentInnerInterface (GenericAgent s res) s res where
    agentRef   = AgentRef
    agentState = _state
    agentResultSuccess a = putResult a . Right
    agentResultFailure a = putResult a . Left . SomeException

putResult a = atomically . putTMVar (_result a)

--------------------------------------------------------------------------------

data EmptyResult res = EmptyResult

data GenericAgentDescriptor s res = GenericAgentDescriptor{
    agName          :: String
  , agDebug         :: Bool
  , messageHandling :: MessageHandling s res
  , action          :: forall c . (AgentInnerInterface c s res) => c -> IO ()
  , initialState    :: IO s
  , emptyResult     :: EmptyResult res
  }

processMessages :: GenericAgent s res -> MessageHandling s res -> IO ()
processMessages a mh = do
      mp <- atomically $    notRunState
                        <|> readMessage _messagePriorityBox
                        <|> readMessage _messageBox
      case mp of Left AgentPause      -> atomically waitNotPause -- TODO
                 Left AgentTerminate  -> throwIO NormalTermination
                 Right msg            -> handleMessage msg
  where notRunState = do state <- readTVar $ _execState a
                         if state == AgentRun then retry
                                              else return $ Left state
        readMessage box = Right <$> readTQueue (box a)
        waitNotPause = do state <- readTVar $ _execState a
                          when (state == AgentPause) retry
        handleMessage msg = case msg
                              of MessageNoResponse m ->
                                     fromMaybe (return ()) (msgHandle mh a m)
                                 MessageAwaitsResponse m respond ->
                                     respond =<< sequence (msgRespond mh a m)


instance CreateAgent (GenericAgentDescriptor s res) res (GenericAgent s res) where
  createAgent d = do  debug     <- newTVarIO $ agDebug d
                      state     <- initialState d
                      exState   <- newTVarIO AgentPause
                      result    <- newEmptyTMVarIO
                      msgBox    <- newTQueueIO
                      msgPBox   <- newTQueueIO
                      msgThreadVar <- newTVarIO undefined
                      actThreadVar <- newTVarIO undefined

                      let a = GenericAgent (agName d) debug
                                           state exState result
                                           msgBox msgPBox
                                           msgThreadVar actThreadVar

                      msgThread <- newAgentThread . forever
                                $  processMessages a (messageHandling d)
                      actThread <- newAgentThread . forever
                                . withExecState a $ action d a
                      atomically $  msgThreadVar `writeTVar` msgThread
                                 >> actThreadVar `writeTVar` actThread
                      return a
    where withExecState a f = do
               execState <- atomically $ do s <- readTVar $ _execState a
                                            if s == AgentPause
                                              then retry
                                              else return s
               case execState
                of AgentRun       -> f
                   AgentTerminate -> throwIO NormalTermination


instance CreateAgentRef (GenericAgentDescriptor s res) res where
    type CreateAgentType (GenericAgentDescriptor s res) = (GenericAgent s res)

--------------------------------------------------------------------------------

mbHandle  :: (AgentInnerInterface c s res, Message msg0, Message msg)
          => (c -> msg  ->        IO ())
          ->  c -> msg0 -> Maybe (IO ())
mbHandle f c msg = f c <$> cast msg

selectMessageHandler  :: (AgentInnerInterface c s res, Message msg)
                      => [c -> msg -> Maybe (IO ())]
                      ->  c -> msg -> Maybe (IO ())
selectMessageHandler rfs c msg  = foldr (<|>) Nothing
                                $ (\f -> f c msg) <$> rfs


--------------------------------------------------------------------------------

mbResp  :: ( AgentInnerInterface c s res
           , MessageResponse msg0 resp0, MessageResponse msg resp )
        => (c -> msg  ->        IO resp)
        ->  c -> msg0 -> Maybe (IO resp0)
mbResp f c msg = do msg' <- cast msg
                    cast $ f c msg'

selectResponse  :: (AgentInnerInterface c s res, MessageResponse msg resp)
                => [c -> msg -> Maybe (IO resp)]
                ->  c -> msg -> Maybe (IO resp)
selectResponse rfs c msg  = foldr (<|>) Nothing
                        $ (\f -> f c msg) <$> rfs

--------------------------------------------------------------------------------
