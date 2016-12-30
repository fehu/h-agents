-----------------------------------------------------------------------------
--
-- Module      :  Agent
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  kdn.kovalev@gmail.com
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

{-# LANGUAGE ExistentialQuantification
           , TypeFamilies
           , FlexibleInstances
           , MultiParamTypeClasses
         #-}

module Agent (

  ExecutableAgent

, agentNoBehaviour

-- , extractAgentStates

, SimpleExecStatus (AgentIsExecuting, AgentIsStopped)

, module A

) where

import Agent.Abstract as A

import Data.Typeable
import Data.Function (on)
import Data.Maybe

import Control.Concurrent
import Control.Concurrent.STM
import Control.Applicative
import Control.Monad



_localDEBUG = True
localDebug scope = when _localDEBUG . putStrLn . (("[DEBUG] " ++ scope ++ ": ") ++)


-- -----------------------------------------------

data Message' = forall msg . Message msg  => Message msg
instance Show Message' where show (Message msg) = show msg

-- -----------------------------------------------

data AgentImpl states execState = AgentImpl {
  _agentId :: AgentId,
  _agentThreads :: TVar AgentThreads,

  _actCtrl  :: TVar (AgentActControl states),
  _msgCtrl  :: TVar (AgentHandleMessages states),

  _actUpdateCtrl    :: TMVar (AgentActControl states),

  _runControlState   :: TVar AgentControlState,
  _runControlUpdate  :: TMVar AgentControlState,

  _execState  :: TVar execState,
  _states     :: states,

  _messageBox          :: TQueue (Either Message' MessageWithResponse),
  _messageBoxPriority  :: TQueue (Either Message' MessageWithResponse),

  _agentDebug  :: Bool
  }

getRunState = readTVar . _runControlState

updRunState ag s = do  debugMsg ag $ "updating Run state: " ++ show s
                       atomically  $ _runControlUpdate ag `putTMVar` s

setExecState ag s = do  debugMsg ag $ "Exec State = " ++ show s
                        atomically $ _execState ag `writeTVar` s

withExecStateRun   ag f  = setExecState ag actionExecuting  >> f
withExecStateStop  ag f  = setExecState ag actionStopped    >> f

-- extractAgentStates :: Typeable s => ExecutableAgent -> Maybe s
-- extractAgentStates (ExecutableAgent ag) = cast $ _states ag


data MessageWithResponse =
    forall msg resp . (Message msg, Message resp, ExpectedResponse msg ~ resp) =>
        MessageWithResponse msg (resp -> IO())


data AgentControlState = AgentRun | AgentPause | AgentTerminate deriving (Show, Eq)


instance Show MessageWithResponse where
    show (MessageWithResponse msg _) = show msg

-- -----------------------------------------------

data ExecutableAgent = forall states execState . (  Typeable states
                                                 ,  AgentActionStatus execState
                                                 ) =>
    ExecutableAgent (AgentImpl states execState)
executableAgentId (ExecutableAgent a) = _agentId a

instance Eq  ExecutableAgent  where (==)     = (==) `on` executableAgentId
instance Ord ExecutableAgent  where compare  = compare `on` executableAgentId

-- -----------------------------------------------


instance AgentComm ExecutableAgent where
    agentId (ExecutableAgent a)  = _agentId a
    send (ExecutableAgent a)     = _writeTQueue a _messageBox . Left . Message
    ask   = _ask MessageWithResponse


_writeTQueue a getBox msg' = atomically $ writeTQueue (getBox a) msg'

_ask mkHook (ExecutableAgent a) msg = do
        respVar <- newEmptyMVar
        _writeTQueue a _messageBox . Right $ mkHook msg (putMVar respVar)
        readMVar respVar


-- -----------------------------------------------

instance AgentCommPriority ExecutableAgent where
    sendPriority (ExecutableAgent a) = _writeTQueue a _messageBoxPriority . Left . Message
    askPriority   = _askPriority MessageWithResponse

_askPriority mkHook (ExecutableAgent a) msg = do
        respVar <- newEmptyMVar
        _writeTQueue a _messageBoxPriority . Right $ mkHook msg (putMVar respVar)
        readMVar respVar

instance AgentStatus ExecutableAgent where
  isActing        (ExecutableAgent a)  = agentIsActing a
  isIdle          (ExecutableAgent a)  = agentIsIdle a
  isTerminated    (ExecutableAgent a)  = agentIsTerminated a
  waitIdle        (ExecutableAgent a)  = agentWaitIdle a
  waitTerminate   (ExecutableAgent a)  = agentWaitTerminate a

instance AgentControl ExecutableAgent where
    startAgent ag   = ag `sendPriority` StartMessage
    stopAgent ag    = ag `send` StopMessage
    stopAgentNow ag = ag `sendPriority` StopMessage


-- -----------------------------------------------


whenM :: (Monad m) => m Bool -> m () -> m ()
whenM mb a = (`when` a) =<< mb


_updateStates ag = do  mbAct  <- tryTakeTMVar $ _actUpdateCtrl ag
                       mbRun  <- tryTakeTMVar $ _runControlUpdate ag

                       let mb = flip $maybe (return ())
                       mb mbAct (_actCtrl ag `writeTVar`)
                       mb mbRun (_runControlState ag `writeTVar`)


-- | Waits update of the state or `upd` variable.
--   Applies the update to agent's variables before returning
--   `Left var update` or `Right run state update`.

_waitUpdate ag upd var =        (lf =<< takeTMVar (upd ag))
                       `orElse` (rf =<< takeTMVar (_runControlUpdate ag))
    where  lf = fmap Left  . sideEffect (var ag `writeTVar`)
           rf = fmap Right . sideEffect (_runControlState ag `writeTVar`)
           sideEffect f x = f x >> return x

-- | Handles messages as defiend in _msgCtrl.
--   Default handlers for 'StartMessage' and 'StopMessage'
--   are predefined, they change agent's act state on 'Running'
--   and 'Terminate' respectfully.

_runAgentMessages :: ExecutableAgent -> IO ()
_runAgentMessages (ExecutableAgent ag) = do
    msg <- atomically $ do  -- _updateStates ag
                            priority  <- tryReadTQueue $ _messageBoxPriority ag
                            runState  <- getRunState ag
                            msg  <- case (priority, runState) of
                                        (Nothing, AgentRun)  -> tryReadTQueue $ _messageBox ag
                                        _                    -> return priority
                            if runState == AgentTerminate   then fail "Terminated"
                                                            else maybe retry return msg
    h <- readTVarIO $ _msgCtrl  ag
    let states = _states ag
        dprint = debugMsg ag
    dprint $ "Message: " ++ show msg
    case msg of  Left (Message msg) ->
                        let  mbStart  = (\StartMessage  -> do dprint "Start message received"
                                                              ag `_start` states
                                        )  <$> cast msg
                             mbStop   = (\StopMessage   -> do dprint "Stop message received"
                                                              ag `_stop` states
                                        )  <$> cast msg
                        in fromMaybe (return ())
                             $   mbStart
                            <|>  mbStop
                            <|>  handleMessage h ag states msg
                 Right (MessageWithResponse msg respond) ->
                        respond =<< fromMaybe
                          (error $ "No response function for " ++ show msg)
                          (respondMessage h ag states msg)

_run  :: (Typeable states) => (AgentControlState -> Bool)
      -> (AgentImpl states exState -> states -> IO ())
      -> AgentImpl states exState  -> states
      -> IO ()
_run atRunState action ag states =
    whenM (atRunState <$> atomically (getRunState ag))
          (action ag states)

-- runs `_act` the corresponding thread thread
_start ::  (Typeable states, AgentActionStatus exState) =>
           AgentImpl states exState -> states -> IO ()
_start = _run (AgentPause ==) $ \ag states -> ag `updRunState` AgentRun

_stop ::  (Typeable states, AgentActionStatus exState) =>
          AgentImpl states exState  -> states -> IO ()
_stop = _run (const True) $ \ag _ -> ag `updRunState` AgentTerminate


_runAgent ag'@(ExecutableAgent ag) = do
    actAndState <- atomically $ (,)  <$> getRunState ag
                                     <*> readTVar (_actCtrl ag)
    let waitActUpd = do  debugMsg ag "act: wait update"
                         upd <- atomically $ _waitUpdate ag _actUpdateCtrl _actCtrl
                         debugMsg ag $  "act: updated: " ++ show upd ++
                                        "; executing runAgent again"
                         _runAgent ag'
        execAct act = act ag (_states ag)

    case actAndState
      of  (AgentTerminate, _)           -> ag `withExecStateStop` fail "Terminated"
          (AgentPause, _)               -> ag `withExecStateStop` waitActUpd
          (_, AgentNoAct)               -> ag `withExecStateStop` waitActUpd
          (_, AgentActOnce act after)   -> do  ag `withExecStateRun`
                                                  atomically (_actCtrl ag `writeTVar` after)
                                               execAct act
          (_, AgentActRepeat act pause) -> do  ag `withExecStateRun` execAct act
                                               maybe (return ()) threadDelay pause


instance (Typeable states, AgentActionStatus exState) =>
  AgentExecControl (AgentImpl states exState) states where
    type ExecutionStatus (AgentImpl states exState) = exState

    agentRef a        = AgentRef (ExecutableAgent a)
    agentFullRef a    = AgentFullRef (ExecutableAgent a)
                      <$> readTVarIO  (_agentThreads a)
    agentTerminate = (`updRunState` AgentTerminate)

    agentExecutionStatus = readTVarIO . _execState
    agentIsTerminated = atomically . fmap (== AgentTerminate) . getRunState

    agentWaitIdle a = atomically $ do  exState <- readTVar $ _execState a
                                       unless  (actionIsStopped exState)
                                               retry

    agentWaitTerminate a = atomically $ do  runState <- getRunState a
                                            unless  (runState == AgentTerminate)
                                                    retry

    actOnce a act = atomically $ do  old <- readTVar $ _actCtrl a
                                     _actUpdateCtrl a `putTMVar` AgentActOnce act old
    actRepeat a act = atomically . putTMVar (_actUpdateCtrl a) . AgentActRepeat act
    actStop = (`updRunState` AgentPause)

    setMessageHandlers arun = atomically . writeTVar (_msgCtrl arun)
    setAgentBehavior arun b = atomically $ do _msgCtrl arun `writeTVar` handleMessages b
                                              _actUpdateCtrl arun `putTMVar` agentAct b
    agentDebug = _agentDebug

-- -----------------------------------------------
-- -----------------------------------------------

agentNoBehaviour = AgentBehavior AgentNoAct $ AgentHandleMessages (\i s -> selectMessageHandler [])
                                                                  (\i s -> selectResponse [])

instance  (  Typeable states
          ,  AgentActionStatus exState
          ) => AgentCreate  (AgentDescriptor states exState)
                            ExecutableAgent
  where
    createAgent AgentDescriptor  {  agentDefaultBehaviour=behaviour
                                 ,  newAgentStates=newStates
                                 ,  initialExecState=iExState
                                 ,  nextAgentId=nextId
                                 ,  debugAgent=debug } =
        do  id        <- nextId
            states    <- newStates

            let iExState' = if iExState == actionStopped
                              then iExState else actionStopped
            a <- atomically $
                do  actCtrl     <- newTVar $ agentAct behaviour
                    actUpdCtrl  <- newEmptyTMVar
                    exState     <- newTVar iExState'
                    threads     <- newTVar undefined
                    msgCtrl     <- newTVar $ handleMessages behaviour
                    runState    <- newTVar AgentPause
                    runStateUpd <- newEmptyTMVar
                    messageBoxPriority  <- newTQueue
                    messageBox          <- newTQueue

                    return AgentImpl {
                                 _agentId        = id,
                                 _agentThreads   = threads,
                                 _agentDebug     = debug,
                                 _states         = states,
                                 _execState      = exState,
                                 _actCtrl        = actCtrl,
                                 _actUpdateCtrl  = actUpdCtrl,
                                 _msgCtrl        = msgCtrl,
                                 _runControlState     = runState,
                                 _runControlUpdate    = runStateUpd,
                                 _messageBox          = messageBox,
                                 _messageBoxPriority  = messageBoxPriority
                               }
            let a'  = ExecutableAgent a

            msgThreadStopped <- newEmptyMVar
            actThreadStopped <- newEmptyMVar

            localDebug "createAgent" "msgThreadId  <- forkFinally"

            -- Start threads
            msgThread  <- forkFinally  (forever $ _runAgentMessages a')
                                       (\_ -> do msgThreadStopped `putMVar` ()
                                                 debugMsg a  "Message Thread Terminated"
                                              )

            localDebug "createAgent" "actThreadId  <- forkFinally"

            actThread  <- forkFinally  (forever $ _runAgent a')
                                       (\_ -> do actThreadStopped `putMVar` ()
                                                 debugMsg a "Act Thread Terminated"
                                       )
            let threads = AgentThreads actThread msgThread
            atomically $ writeTVar (_agentThreads a) threads
            return (a', AgentFullRef a' threads)


-- -----------------------------------------------
-- -----------------------------------------------

data SimpleExecStatus = AgentIsExecuting | AgentIsStopped
  deriving (Show, Eq)

instance AgentActionStatus SimpleExecStatus where
  actionExecuting  = AgentIsExecuting
  actionStopped    = AgentIsStopped
