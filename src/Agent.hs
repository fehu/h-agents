{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Agent(

  ReactiveAgent(..)
, Message, Response, ExpectedResponse, MessageResponse
, handleResponseAsync, handleResponseSuccessAsync
, waitResponse, waitResponseSuccess

, AgentRef, AgentControl(..), AgentInnerInterface(..)

, CreateAgent(..), CreateAgentRef(..)

, GenericAgent
, GenericAgentDescriptor(..), MessageHandling(..)

, mbHandle, selectMessageHandler
, mbResp, selectResponse

) where

import Data.Typeable (Typeable, cast)
import Data.Maybe (fromMaybe, fromJust)

import Control.Applicative ((<|>))
import Control.Monad (forever, when)
import Control.Exception (Exception, SomeException, throwIO)

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

        agentName     :: a -> String
        agentDebug    :: a -> IO Bool
        agentSetDebug :: a -> Bool -> IO ()
        printDebug    :: (Show x) => a -> x -> IO ()

        printDebug a x = do debug <- agentDebug a
                            when debug . putStrLn $
                              "[DEBUG][" ++ show (agentName a) ++ "] " ++ show x

--------------------------------------------------------------------------------

-- | Reference for 'ReactiveAgent'.
data AgentRef = forall a . (ReactiveAgent a, AgentControl a) => AgentRef a

instance ReactiveAgent AgentRef where
  send (AgentRef a) = send a
  ask  (AgentRef a) = ask a
  sendPriority (AgentRef a) = sendPriority a
  askPriority  (AgentRef a) = askPriority a

instance AgentControl AgentRef where
  agentName            (AgentRef a) = agentName a
  agentDebug           (AgentRef a) = agentDebug a
  agentSetDebug        (AgentRef a) = agentSetDebug a
  agentStart           (AgentRef a) = agentStart a
  agentPause           (AgentRef a) = agentPause a
  agentTerminate       (AgentRef a) = agentTerminate a
  agentWaitTermination (AgentRef a) = agentWaitTermination a

--------------------------------------------------------------------------------

-- | Agent is created from the corresponding descriptor.
class CreateAgent from a where
  createAgent :: from -> IO a

class ( CreateAgent from (CreateAgentType from)
      , ReactiveAgent    (CreateAgentType from)
      , AgentControl     (CreateAgentType from)
      ) =>
  CreateAgentRef from where
    type CreateAgentType from :: *
    createAgentRef :: from -> IO AgentRef
    createAgentRef from = do (ag :: CreateAgentType from) <- createAgent from
                             return $ AgentRef ag

--------------------------------------------------------------------------------

class (AgentControl a) =>
  AgentInnerInterface a s | a -> s
    where
      agentRef   :: a -> AgentRef
      agentState :: a -> s


data AgentExecState = AgentRun | AgentPause | AgentTerminate
  deriving (Show, Eq)

data NormalTermination = NormalTermination deriving (Show, Typeable)
instance Exception NormalTermination

--------------------------------------------------------------------------------

data ReceivedMessage  = forall msg . Message msg =>
                               MessageNoResponse msg
                      | forall msg resp . MessageResponse msg resp =>
                               MessageAwaitsResponse msg (Maybe resp -> IO ())

-- | Active and Reactive agent implementation.
--   Has internal state 's'.
--   Message handling and action run in separate threads.
data GenericAgent s = GenericAgent {
    _name               :: String
  , _debug              :: TVar Bool
  , _state              :: s
  , _execState          :: TVar AgentExecState
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
                                                 . either Just (const Nothing)
                       return $ AgentThread threadId threadTerminated

awaitAgentThread :: AgentThread -> IO (Maybe SomeException)
awaitAgentThread (AgentThread _ terminated) = atomically $ takeTMVar terminated

putMessageInBox box msg = atomically $ box `writeTQueue` MessageNoResponse msg
putMessageRespInBox box msg = do
  respBox <- newEmptyTMVarIO
  let  respond = atomically . putTMVar respBox
  atomically $ box `writeTQueue` MessageAwaitsResponse msg respond
  return $ Response respBox

setExecState exState a = atomically $ _execState a `writeTVar` exState

data MessageHandling s = MessageHandling{
    msgHandle   :: forall c msg . (AgentInnerInterface c s, Message msg) =>
                    c -> msg -> Maybe (IO ())
  , msgRespond  :: forall c msg resp . ( AgentInnerInterface c s
                                       , MessageResponse msg resp) =>
                    c -> msg -> Maybe (IO resp)
  }


instance ReactiveAgent (GenericAgent s) where
  send a = putMessageInBox (_messageBox a)
  ask a  = putMessageRespInBox (_messageBox a)

  sendPriority a = putMessageInBox (_messagePriorityBox a)
  askPriority a  = putMessageRespInBox (_messagePriorityBox a)

instance AgentControl (GenericAgent s) where
  agentName       = _name
  agentDebug      = readTVarIO . _debug
  agentSetDebug a = atomically . writeTVar (_debug a)
  agentStart      = setExecState AgentRun
  agentPause      = setExecState AgentPause
  agentTerminate  = setExecState AgentTerminate
  agentWaitTermination a = do waitAndReportTerminationReason _messageThread
                              waitAndReportTerminationReason _actionThread
    where waitAndReportTerminationReason getThreadVar = do
            thread <- readTVarIO $ getThreadVar a
            reason <- awaitAgentThread thread
            printDebug a $ "Terminated " ++
                            maybe "normally."
                                  (\err -> "with error: " ++ show err ++ ".")
                                  reason

instance AgentInnerInterface (GenericAgent s) s where
  agentRef    = AgentRef
  agentState  = _state

--------------------------------------------------------------------------------

data GenericAgentDescriptor s = GenericAgentDescriptor{
    agName          :: String
  , agDebug         :: Bool
  , messageHandling :: MessageHandling s
  , action          :: forall c . (AgentInnerInterface c s) => c -> IO ()
  , initialState    :: IO s
  }

withExecState a f = do execState <- atomically $ do s <- readTVar $ _execState a
                                                    if s == AgentPause
                                                      then retry
                                                      else return s
                       case execState
                        of AgentRun       -> f
                           AgentTerminate -> throwIO NormalTermination

processMessages a mh = do
  msg <- atomically $   readTQueue (_messagePriorityBox a)
                    <|> readTQueue (_messageBox a)
  case msg of MessageNoResponse m ->
                  fromMaybe (return ()) (msgHandle mh a m)
              MessageAwaitsResponse m respond ->
                  respond =<< sequence (msgRespond mh a m)

instance CreateAgent (GenericAgentDescriptor s) (GenericAgent s) where
  createAgent d = do  debug     <- newTVarIO $ agDebug d
                      state     <- initialState d
                      exState   <- newTVarIO AgentPause
                      msgBox    <- newTQueueIO
                      msgPBox   <- newTQueueIO
                      msgThreadVar <- newTVarIO undefined
                      actThreadVar <- newTVarIO undefined

                      let a = GenericAgent (agName d) debug
                                           state exState
                                           msgBox msgPBox
                                           msgThreadVar actThreadVar

                      msgThread <- newAgentThread
                                .  foreverWithExecState a
                                $  processMessages a (messageHandling d)
                      actThread <- newAgentThread
                                .  foreverWithExecState a
                                $  action d a
                      atomically $  msgThreadVar `writeTVar` msgThread
                                 >> actThreadVar `writeTVar` actThread
                      return a
    where foreverWithExecState a = forever . withExecState a

instance CreateAgentRef (GenericAgentDescriptor s) where
  type CreateAgentType (GenericAgentDescriptor s) = (GenericAgent s)

--------------------------------------------------------------------------------

mbHandle  :: (AgentInnerInterface c s, Message msg0, Message msg)
          => (c -> msg  ->        IO ())
          ->  c -> msg0 -> Maybe (IO ())
mbHandle f c msg = f c <$> cast msg

selectMessageHandler  :: (AgentInnerInterface c s, Message msg)
                      => [c -> msg -> Maybe (IO ())]
                      ->  c -> msg -> Maybe (IO ())
selectMessageHandler rfs c msg  = foldr (<|>) Nothing
                                $ (\f -> f c msg) <$> rfs


--------------------------------------------------------------------------------

mbResp  :: ( AgentInnerInterface c s
           , MessageResponse msg0 resp0, MessageResponse msg resp )
        => (c -> msg  ->        IO resp)
        ->  c -> msg0 -> Maybe (IO resp0)
mbResp f c msg = do msg' <- cast msg
                    cast $ f c msg'

selectResponse  :: (AgentInnerInterface c s, MessageResponse msg resp)
                => [c -> msg -> Maybe (IO resp)]
                ->  c -> msg -> Maybe (IO resp)
selectResponse rfs c msg  = foldr (<|>) Nothing
                        $ (\f -> f c msg) <$> rfs

--------------------------------------------------------------------------------
