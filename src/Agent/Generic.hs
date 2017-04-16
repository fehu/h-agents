-----------------------------------------------------------------------------
--
-- Module      :  Agent.Generic
-- License     :  MIT
--
-- Maintainer  :  kdn.kovalev@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable (requires STM)
--
-----------------------------------------------------------------------------

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Agent.Generic(

  GenericAgent
, GenericAgentDescriptor(..)

, EmptyResult(..)

, module Export

) where

import Agent            as Export
import Agent.Behavior   as Export
import Agent.Message

import Data.Maybe (isNothing, fromMaybe)
import Data.Typeable (Typeable, cast)

import Control.Applicative ( (<|>) )
import Control.Monad (when, unless, forever)
import Control.Exception (Exception, SomeException(..), throwIO)
import Control.Concurrent (ThreadId, forkFinally)
import Control.Concurrent.STM


-----------------------------------------------------------------------------

-- | Active and Reactive agent implementation.
--   Has internal state 's'.
--   Message handling and action run in separate threads.
data GenericAgent s res = GenericAgent {
    _name               :: AgentId
  , _debug              :: TVar Bool
  , _state              :: s
  , _execState          :: TVar AgentExecState
  , _result             :: TMVar (AgentExecutionResult res)
  , _messageBox         :: TQueue ReceivedMessage
  , _messagePriorityBox :: TQueue ReceivedMessage
  , _messageThread      :: TVar AgentThread
  , _actionThread       :: TVar AgentThread
  }

data AgentExecState = AgentRun | AgentPause | AgentTerminate
  deriving (Show, Eq)

data ReceivedMessage  = forall msg . Message msg =>
                               MessageNoResponse msg
                      | forall msg resp . MessageResponse msg resp =>
                             MessageAwaitsResponse msg (Respond resp)


data AgentThread = AgentThread ThreadId (TMVar (Maybe SomeException))

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

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

-----------------------------------------------------------------------------

data NormalTermination = NormalTermination deriving (Show, Typeable)
instance Exception NormalTermination

-----------------------------------------------------------------------------

data AgentThreadsException =
  forall exM exA . (Exception exA, Exception exM) => AgentThreadsException exM exA
  deriving Typeable
instance Show AgentThreadsException where
  show (AgentThreadsException exA exM) = showActionException  exA ++ "\n" ++
                                         showMessageException exM
instance Exception AgentThreadsException


showActionException  ex = "Exception in action thread: "  ++ show ex
showMessageException ex = "Exception in message thread: " ++ show ex


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

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

instance ResponsePromise (GenericAgent s res) Response Respond where
  promiseResponse ag = do respVar <- newEmptyTMVarIO
                          return ( Response . atomically $ takeTMVar respVar
                                 , Respond respVar )

instance ReactiveAgent (GenericAgent s res) where
  send = putMessageInBox _messageBox
  ask  = putMessageRespInBox _messageBox

  sendPriority = putMessageInBox _messagePriorityBox
  askPriority  = putMessageRespInBox _messagePriorityBox


putMessageInBox fbox a msg = atomically $
                             fbox a `writeTQueue` MessageNoResponse msg
putMessageRespInBox fbox a msg = do
    (promise, resp) <- promiseResponse a
    atomically $ fbox a `writeTQueue` MessageAwaitsResponse msg resp
    return promise

-----------------------------------------------------------------------------

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


setExecState exState a = atomically $ _execState a `writeTVar` exState
isExecState state = fmap (== state) . readTVarIO . _execState
abortedExceptionResult = Left $ SomeException AgentExecutionAborted

-----------------------------------------------------------------------------

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
    agentRef   = agentRef
    agentState = _state
    agentResultSuccess a = putResult a . Right
    agentResultFailure a = putResult a . Left . SomeException

putResult a = atomically . putTMVar (_result a)

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

data GenericAgentDescriptor s res = GenericAgentDescriptor{
    agName          :: String
  , agDebug         :: Bool
  , messageHandling :: MessageHandling s res
  , action          :: AgentAction s res
  , initialState    :: IO s
  , emptyResult     :: EmptyResult res
  }

data EmptyResult res = EmptyResult

-----------------------------------------------------------------------------

instance CreateAgent (GenericAgentDescriptor s res) res (GenericAgent s res) where
  createAgent d = do  debug     <- newTVarIO $ agDebug d
                      state     <- initialState d
                      exState   <- newTVarIO AgentPause
                      result    <- newEmptyTMVarIO
                      msgBox    <- newTQueueIO
                      msgPBox   <- newTQueueIO
                      msgThreadVar <- newTVarIO undefined
                      actThreadVar <- newTVarIO undefined

                      let a = GenericAgent (AgentId $ agName d) debug
                                           state exState result
                                           msgBox msgPBox
                                           msgThreadVar actThreadVar

                      msgThread <- newAgentThread . forever
                                $  processMessages a (messageHandling d)
                      actThread <- newAgentThread . forever
                                . withExecState a $ runAgentAction (action d) a
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
  type CreateAgentType (GenericAgentDescriptor s res) = GenericAgent s res

-----------------------------------------------------------------------------

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
                                 MessageAwaitsResponse m provider ->
                                     (`provideResponse` provider) =<<
                                     sequence (msgRespond mh a m)

-----------------------------------------------------------------------------
