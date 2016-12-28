%if False
\begin{code}

{-# LANGUAGE ExistentialQuantification
           , TupleSections
           , Rank2Types
           , FlexibleContexts
           , FlexibleInstances
           , MultiParamTypeClasses
        #-}

module Agent.AgentSystem.Controller.Leaf (

  AgentsManager, ManagerState(..)
, ManagerDescriptor(..), AgentsManagerBuilder
, agentsManagerBuilder

, defaultManagerDescriptor

) where

  import Control.Monad
  import Control.Concurrent.STM

  import Data.Typeable
  import Data.IORef
  import Data.Function (on)
  import Data.Maybe (isJust, fromJust)
  import Data.Either (partitionEithers, isLeft)
  import Data.Map (Map)

  import qualified Data.Map as Map

  import Agent.Abstract
  import Agent.AgentSystem.Controller.AgentStatus
  import Agent.AgentSystem.Controller.Interface


  -- _local_DEBUG = True
  -- localDebug scope = when _local_DEBUG . putStrLn . (("[DEBUG] " ++ scope ++ ": ") ++)

\end{code}
%endif

\verb|ControllerLeaf| implementation --- \verb|AgentsManager|.

% A \emph{leaf} controller holds references to the controlled agents. During
% system execution, it creates and monitors the agents, notifying the hierarchy
% about agents' \verb|Status| changes.

Leaf controller:

\begin{code}

  type AgentStatusMap' res = Map AgentFullRef (TVar (AgentStatus res))

  newtype AgentsManager res = AgentsManager ( AgentFullRef
                                            , Maybe (SomeController res)
                                            )

  agentsManagerId (AgentsManager (ref,_)) = agentId ref

  data ManagerDescriptor res = ManagerDescriptor{
    managerName  :: String,
    managerPause :: Maybe Millis,
    managerMonitorStatus :: AgentStatusMap res -> IO (Maybe ControllerNotification),
    -- managerProcessNotification  :: ManagerState res -> ControllerNotification -> IO (),
    managerProcessMessage       :: forall msg . Message msg => ManagerState res -> msg -> Maybe (IO ()),
    managerRespondMessage       :: forall msg . ( Message msg
                                                , Message (ExpectedResponse msg)) =>
                                ManagerState res -> msg -> Maybe (IO (ExpectedResponse msg))
  }

  data ManagerState res = ManagerState{
    managerAgents :: IORef (AgentStatusMap' res),
    managerParent :: Maybe (SomeController res)
  }


  type AgentsManagerBuilder res  = Maybe (SomeController res)
                                 -> IO (AgentDescriptor (ManagerState res) ())

  agentsManagerBuilder  :: (Typeable res) =>
                           ManagerDescriptor res
                        -> Bool
                        -> AgentsManagerBuilder res
  agentsManagerBuilder d debug mbParent =
      newAgentDescriptor  (managerName d) behavior
                          (newStates mbParent) () debug

    where behavior = AgentBehavior{
                       agentAct = AgentActRepeat (const monitorAgents) (managerPause d),
                       handleMessages = AgentHandleMessages {
                                      handleMessage = const $ managerProcessMessage d,
                                      respondMessage = const $ managerRespondMessage d
                                      }
                     }
          monitorAgents s = do  ags   <- readIORef $ managerAgents s
                                let ags' = Map.map readTVar ags
                                mbMsg <- managerMonitorStatus d ags'
                                forM_ mbMsg $ notifyParent s
          notifyParent s msg = mapM_ (`send` msg) (managerParent s)
          newStates mbParent = do  ags <- newIORef Map.empty
                                   return $ ManagerState ags mbParent


  -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

  instance Eq  (AgentsManager res) where (==) = (==) `on` agentsManagerId
  instance Ord (AgentsManager res) where compare = compare `on` agentsManagerId

  instance (Typeable res) => AgentComm (AgentsManager res) where
    agentId = agentsManagerId
    send (AgentsManager (ref,_)) = send ref
    ask  (AgentsManager (ref,_)) = ask ref

  instance (Typeable res) => AgentCommPriority (AgentsManager res) where
    sendPriority (AgentsManager (ref,_)) = sendPriority ref
    askPriority  (AgentsManager (ref,_)) = askPriority ref

  instance (Typeable res) => Controller (AgentsManager res) res where
    parentController (AgentsManager (_, parent)) = parent

    controlledAgents (AgentsManager (ref, _)) =
      unwrapAgentsList <$> ref `ask` ListAgents

    tryGetResults c = do  res' <- optResults c
                          return $  if all isJust $ Map.elems res'
                                    then Just $ Map.map fromJust res'
                                    else Nothing

    waitResults c = atomically . flip waitResults' [] =<< Map.assocs <$> controlledAgents c

    stopControlledAgent c ref = do  ags' <- controlledAgents c
                                    case Map.lookup ref ags'
                                      of Just state  -> do  stopAgentNow ref
                                                            waitTerminate state


    stopController c = do
      ags' <- controlledAgents c
      mapM_ stopAgentNow $ Map.keys ags'
      c `send` StopMessage
      notifyParent c TerminatedByRequest



  unwrapAgentsList (AgentsList ags) = fromJust $ cast ags

  statusResult s = case s of  Waiting res   -> Just (Right res)
                              Locked res    -> Just (Right res)
                              Terminated e  -> Just (Left e)
                              _             -> Nothing

  optResults c = do
    ags' <- controlledAgents c
    optResults' ags'

  -- reads all the states at once
  optResults' ags' = do
    ags  <- atomically . forM (Map.assocs ags') $ \(ref,stm) -> (ref,) <$> stm
    return . Map.fromList $ do  (ref, state) <- ags
                                return (ref, statusResult state)


  waitResults' [] acc = return $ Map.fromList acc
  waitResults' ((ref,stateSTM):ags') acc = do
    state <- stateSTM
    case statusResult state of  Just res  -> waitResults' ags' ((ref,res):acc)
                                Nothing   -> retry

  waitTerminate = atomically . waitTerminate'
  waitTerminate' stateSTM = do
    state <- stateSTM
    case state of  Terminated _  -> return ()
                   _             -> retry

  notifyParent c msg = forM_ (parentController c) (`send` msg)

  instance (Typeable res) => ControllerLeaf (AgentsManager res) res where
    newAgentsL c = fmap unwrapAgentsList . ask c . map CreateAgent'

\end{code}

Default \verb|ManagerDescriptor|:

\begin{code}

  defaultManagerDescriptor :: (Typeable res) =>
                           String -> Maybe Millis -> res -> ManagerDescriptor res
  defaultManagerDescriptor name pause emptyResult = ManagerDescriptor
    {  managerName = name
    ,  managerPause = pause
    ,  managerMonitorStatus = \sm -> do
         optRes <- optResults' sm
         return $
            if all isJust $ Map.elems optRes
            then  let (errs', res') = Map.partition isLeft $ Map.map fromJust optRes
                  in Just $  if Map.null errs'
                             then HaveAllResults
                             else AgentsFailed $ Map.map (\(Left ex) -> ex) errs'
            else Nothing
    ,  managerProcessMessage = \state -> selectMessageHandler []
    ,  managerRespondMessage = selectResponse'
            [  mbResp' handleAgentsList
            ,  mbResp' $ handleAgentsCreation []
            ]
    }

  handleAgentsList state ListAgents  = do
    ags'  <- readIORef (managerAgents state)
    ags   <- sequence $ do  (ref, s') <- Map.assocs ags'
                            return $ (ref,) <$> readTVarIO s'
    return . AgentsList $ Map.fromList ags


  handleAgentsCreation acc _ [] = return . AgentsList $ Map.fromList acc
  handleAgentsCreation acc ms
    (CreateAgent' (CreateAgent descr extractStatus) : cs)
    =  do  (agent, ref) <- createAgent descr
           let  status' = extractStatus agent
                status = fromJust $ cast status'
           modifyIORef (managerAgents ms) (Map.insert ref status)
           s <- readTVarIO status
           handleAgentsCreation ((ref, s) : acc) ms cs

\end{code}
