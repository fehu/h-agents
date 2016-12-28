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

, extractAgentStates

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

data AgentRun states = AgentRun {
  _agentId :: AgentId,

  _actCtrl  :: TVar (AgentActControl states),
  _msgCtrl  :: TVar (AgentHandleMessages states),

  _actUpdateCtrl   :: TMVar (AgentActControl states),

  _runState        :: TVar RunState,
  _runStateUpdate  :: TMVar RunState,

  _states    :: states,

  _messageBox          :: TQueue (Either Message' MessageWithResponse),
  _messageBoxPriority  :: TQueue (Either Message' MessageWithResponse),

  _agentDebug  :: Bool
  }

getRunState = readTVar . _runState

updRunState ag s = do  debugMsg ag $ "updating Run state: " ++ show s
                       atomically  $ _runStateUpdate ag `putTMVar` s


extractAgentStates :: Typeable s => ExecutableAgent -> Maybe s
extractAgentStates (ExecutableAgent ag) = cast $ _states ag


data MessageWithResponse =
    forall msg resp . (Message msg, Message resp, ExpectedResponse msg ~ resp) =>
        MessageWithResponse msg (resp -> IO())


data RunState = Paused | Running | Terminate deriving (Show, Eq)


instance Show MessageWithResponse where
    show (MessageWithResponse msg _) = show msg

-- -----------------------------------------------

data ExecutableAgent = forall states . Typeable states => ExecutableAgent (AgentRun states)
executableAgentId (ExecutableAgent run) = _agentId run

instance Eq  ExecutableAgent  where (==)     = (==) `on` executableAgentId
instance Ord ExecutableAgent  where compare  = compare `on` executableAgentId

-- -----------------------------------------------


instance AgentComm ExecutableAgent where
    agentId (ExecutableAgent run)  = _agentId run
    send (ExecutableAgent run)     = _writeTQueue run _messageBox . Left . Message
    ask   = _ask MessageWithResponse


_writeTQueue run getBox msg' = atomically $ writeTQueue (getBox run) msg'

_ask mkHook (ExecutableAgent run) msg = do
        respVar <- newEmptyMVar
        _writeTQueue run _messageBox . Right $ mkHook msg (putMVar respVar)
        readMVar respVar


-- -----------------------------------------------

instance AgentCommPriority ExecutableAgent where
    sendPriority (ExecutableAgent run) = _writeTQueue run _messageBoxPriority . Left . Message
    askPriority   = _askPriority MessageWithResponse

_askPriority mkHook (ExecutableAgent run) msg = do
        respVar <- newEmptyMVar
        _writeTQueue run _messageBoxPriority . Right $ mkHook msg (putMVar respVar)
        readMVar respVar


instance AgentControl ExecutableAgent where
    startAgent ag   = ag `sendPriority` StartMessage
    stopAgent ag    = ag `send` StopMessage
    stopAgentNow ag = ag `sendPriority` StopMessage


-- -----------------------------------------------


whenM :: (Monad m) => m Bool -> m () -> m ()
whenM mb a = (`when` a) =<< mb


_updateStates ag = do  mbAct  <- tryTakeTMVar $ _actUpdateCtrl ag
                       mbRun  <- tryTakeTMVar $ _runStateUpdate ag

                       let mb = flip $maybe (return ())
                       mb mbAct (_actCtrl ag `writeTVar`)
                       mb mbRun (_runState ag `writeTVar`)


-- | Waits update of the state or `upd` variable.
--   Applies the update to agent's variables before returning
--   `Left var update` or `Right run state update`.

_waitUpdate ag upd var =        (lf =<< takeTMVar (upd ag))
                       `orElse` (rf =<< takeTMVar (_runStateUpdate ag))
    where  lf = fmap Left  . sideEffect (var ag `writeTVar`)
           rf = fmap Right . sideEffect (_runState ag `writeTVar`)
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
                                        (Nothing, Running)  -> tryReadTQueue $ _messageBox ag
                                        _                   -> return priority
                            if runState == Terminate  then fail "Terminated"
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

_run  :: (Typeable states) => (RunState -> Bool)
      -> (AgentRun states -> states -> IO ())
      -> AgentRun states -> states
      -> IO ()
_run atRunState action ag states =
    whenM (atRunState <$> atomically (getRunState ag))
          (action ag states)

-- runs `_act` the corresponding thread thread
_start :: (Typeable states) => AgentRun states -> states -> IO ()
_start = _run (Paused ==) $ \ag states -> ag `updRunState` Running

_stop :: (Typeable states) => AgentRun states -> states -> IO ()
_stop = _run (const True) $ \ag _ -> ag `updRunState` Terminate


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
      of  (Terminate, _)                -> fail "Terminated"
          (Paused, _)                   -> waitActUpd
          (_, AgentNoAct)               -> waitActUpd
          (_, AgentActOnce act after)   -> atomically (_actCtrl ag `writeTVar` after) >> execAct act
          (_, AgentActRepeat act pause) -> execAct act >> maybe (return ()) threadDelay pause


instance (Typeable states) => AgentExecControl (AgentRun states) states where
    agentRef arun        = AgentRef (ExecutableAgent arun)
    agentTerminate arun  = arun `updRunState` Terminate

    actOnce arun act = atomically $ do  old <- readTVar $ _actCtrl arun
                                        _actUpdateCtrl arun `putTMVar` AgentActOnce act old
    actRepeat arun act = atomically . putTMVar (_actUpdateCtrl arun) . AgentActRepeat act
    actPause arun = arun `updRunState` Paused

    setMessageHandlers arun = atomically . writeTVar (_msgCtrl arun)
    setAgentBehavior arun b = atomically $ do _msgCtrl arun `writeTVar` handleMessages b
                                              _actUpdateCtrl arun `putTMVar` agentAct b
    agentDebug = _agentDebug

-- -----------------------------------------------
-- -----------------------------------------------

agentNoBehaviour = AgentBehavior AgentNoAct $ AgentHandleMessages (\i s -> selectMessageHandler [])
                                                                  (\i s -> selectResponse [])

instance (Typeable states) => AgentCreate  (AgentDescriptor states res)
                                           ExecutableAgent
  where
    createAgent AgentDescriptor  {  agentDefaultBehaviour=behaviour
                                 ,  newAgentStates=newStates
                                 ,  nextAgentId=nextId
                                 ,  debugAgent=debug } =
        do  id        <- nextId
            states    <- newStates

            run <- atomically $
                do  actCtrl     <- newTVar $ agentAct behaviour
                    actUpdCtrl  <- newEmptyTMVar
                    msgCtrl     <- newTVar $ handleMessages behaviour
                    runState    <- newTVar Paused
                    runStateUpd <- newEmptyTMVar
                    messageBoxPriority  <- newTQueue
                    messageBox          <- newTQueue

                    return AgentRun {
                                 _agentId       = id,
                                 _agentDebug    = debug,
                                 _states        = states,
                                 _actCtrl       = actCtrl,
                                 _actUpdateCtrl = actUpdCtrl,
                                 _msgCtrl       = msgCtrl,
                                 _runState      = runState,
                                 _runStateUpdate        = runStateUpd,
                                 _messageBox            = messageBox,
                                 _messageBoxPriority    = messageBoxPriority
                               }
            let run'  = ExecutableAgent run

            msgThreadStopped <- newEmptyMVar
            actThreadStopped <- newEmptyMVar

            localDebug "createAgent" "msgThreadId  <- forkFinally"

            -- Start threads
            msgThreadId  <- forkFinally  (forever $ _runAgentMessages run')
                                         (\_ -> do msgThreadStopped `putMVar` undefined
                                                   debugMsg run  "Message Thread Terminated"
                                                )

            localDebug "createAgent" "actThreadId  <- forkFinally"

            actThreadId  <- forkFinally  (forever $ _runAgent run')
                                         (\_ -> do actThreadStopped `putMVar` undefined
                                                   debugMsg run "Act Thread Terminated"
                                         )

            let msgThread = AgentThread {  _threadId       = msgThreadId
                                        ,  _threadFinished = not <$> isEmptyMVar msgThreadStopped
                                        ,  _waitThread     = readMVar msgThreadStopped
                                        }
                actThread = AgentThread {  _threadId       = actThreadId
                                        ,  _threadFinished = not <$> isEmptyMVar actThreadStopped
                                        ,  _waitThread     = readMVar actThreadStopped
                                        }
                threads = AgentThreads actThread msgThread

            return (run', AgentFullRef run' threads)
