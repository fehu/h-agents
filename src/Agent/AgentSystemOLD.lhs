%if standalone
\documentclass{article}

%include src/Document/format.fmt
%include polycode.fmt
%include forall.fmt

\usepackage[english]{babel}
\usepackage[inline, shortlabels]{enumitem}

\usepackage{showframe}

\begin{document}
%endif

%if False
\begin{code}

module Agent.AgentSystem (

  AgentSystem(..)
, AgentRoleDescriptor(..)
, SomeRole(..)
, RootRole(..), RootController

, AgentsSysCtrl, NoResult(..)

, SystemExecutionSuccess, SystemExecutionFailure, SystemExecutionResult

,  module Export

) where

  import Control.Monad
  import Control.Concurrent.STM
  import Control.Exception (SomeException)

  import Data.Typeable
  import Data.Map (Map)
  import Data.Maybe (fromJust)
  import Data.Time.Clock

  import qualified Data.Map as Map

  import Agent as Export
  import Agent.AgentSystem.Controller as Export


  -- _local_DEBUG = True
  -- localDebug scope = when _local_DEBUG . putStrLn . (("[DEBUG] " ++ scope ++ ": ") ++)

\end{code}
%endif



\red{rewrite it}

An \emph{agent system} uses agent \emph{roles} to create control hierarchy.
Furthermore, roles serve as agents \emph{archetypes}, defining agents' states
structure and behavior traits.


> class AgentSystem sys res | sys -> res where
>   createAgentSystem :: Bool -> IO sys

>   newAgents :: (AgentRoleDescriptor r res)  => sys
>                                             -> r
>                                             -> [RoleInput r]
>                                             -> IO [AgentFullRef]
>   listAgents       :: sys -> IO (Map AnyRole [AgentFullRef])
>   listAgentStates  :: sys -> IO (Map AnyRole (AgentStatusMap res))

>   startAgents :: sys -> IO ()
>   stopAgents  :: sys -> IO ()

% >   terminateAgents       :: sys -> IO ()
% >   terminateAgentsForce  :: sys -> IO ()

>   agentsStarted :: sys -> IO (Maybe UTCTime)
>   agentsStopped :: sys -> IO (Maybe UTCTime)

>   tryGetResult :: sys -> IO (Maybe (SystemExecutionResult res))
>   waitResult   :: sys -> IO (SystemExecutionResult res)


> type SystemExecutionSuccess res  = Map AgentRef res
> type SystemExecutionFailure res  = Map AgentRef SomeException
> type SystemExecutionResult  res  = Either  (SystemExecutionFailure res)
>                                            (SystemExecutionSuccess res)



% >   mapAgents        :: (AgentFullRef -> IO a)   -> m -> IO [a]
% >   mapAgents_       :: (AgentFullRef -> IO ())  -> m -> IO ()
%
% >   mapAgentStates   :: ((AgentFullRef, s) -> IO a)   -> m -> IO [a]
% >   mapAgentStates_  :: ((AgentFullRef, s) -> IO ())  -> m -> IO ()
%
% >   foreachAgent     :: m -> (AgentFullRef -> IO a)   -> IO [a]
% >   foreachAgent_    :: m -> (AgentFullRef -> IO ())  -> IO ()
%
% >   foreachAgentState   :: m -> ((AgentFullRef, s)  -> IO a)   -> IO [a]
% >   foreachAgentState_  :: m -> ((AgentFullRef, s)  -> IO ())  -> IO ()
%
% >   mapAgents f   = mapM f   <=< listAgents
% >   mapAgents_ f  = mapM_ f  <=< listAgents
% >   mapAgentStates f   = mapM f   <=< listAgentStates
% >   mapAgentStates_ f  = mapM_ f  <=< listAgentStates
%
% >   foreachAgent   = flip mapAgents
% >   foreachAgent_  = flip mapAgents_
% >   foreachAgentState   = flip mapAgentStates
% >   foreachAgentState_  = flip mapAgentStates_




> class (RoleIx r, Show r, Typeable r, Typeable (RoleStates r)) =>
>   AgentRoleDescriptor r res | r -> res where
>     type RoleInput  r :: *
>     type RoleStates r :: *
>     roleStates     :: r -> RoleInput r   -> IO (RoleStates r)
>     roleStatesExt  :: r -> RoleStates r  -> AgentStatus' res
>     roleBehavior   :: r -> RoleInput r   -> AgentBehavior (RoleStates r)
>     nextRoleId     :: r -> IO AgentId

% >     roleStatesExt  :: r -> RoleStates r  -> AgentStatus' res

> data SomeRole res = forall r . ( AgentRoleDescriptor r res
>                                , RoleIx r ) => SomeRole r
>   deriving Typeable
> instance Show (SomeRole res) where show (SomeRole r) = show r

% > instance RoleIx (SomeRole res) where

> anyRole (SomeRole r) = AnyRole r

> data RootRole res = RootRole

\verb|AgentSystem| creates an hierarchy, where all agents of the same role
are controlled by a unique \verb|ControllerLeaf|. The only \verb|ControllerNode|
is system's root. The leafs should monitor their agents' execution states and report
changes to the root.
% The following rules apply to the system's life cycle:
% \begin{itemize}
%   \item An error during any agent's execution (that is denoted by \verb|Terminated|
%     \verb|AgentStatus|) causes termination of all system's agents.
%   \item When an agent finishes all its jobs, it changes its status to \verb|Waiting|.
%     A leaf controller waits for all the agent to take \verb|Waiting| state; then
%     it sends \verb|Lock| command message to the agents.
%   \item After receiving \verb|Lock| message, an agent should fix its result and
%    change the state to \verb|Locked|. The corresponding leaf controller waits for
%    all the agent to set this status, then collects their results and propagates
%    them to the root.
% \end{itemize}

\verb|AgentSystem| implementation:

\begin{code}

  data AgentsSysCtrl result = AgentsSysCtrl {
      _agentSysExecBegan   :: TMVar UTCTime,
      _agentSysExecEnded   :: TMVar UTCTime,
      _agentSysExecResult  :: TMVar (SystemExecutionResult result),

      _agentSysDebug       :: Bool,
      rootController       :: RootController result
  }

  type RootController res = AgentsOverseer (SomeRole res) res

  class NoResult res where noResult' :: res

  instance ( Typeable res, NoResult res ) =>
    AgentSystem (AgentsSysCtrl res) res where
      newAgents sys r ris = fmap (map fst) $
        newAgentsN (rootController sys) (SomeRole r)
        $ do  ri <- ris

              let  descr = AgentDescriptor
                             (roleBehavior r ri)
                             (roleStates r ri)
                             (nextRoleId r)
                             noResult'
                             (_agentSysDebug sys)

              return $ crAg r descr (roleStatesExt r)

      listAgents = fmap (Map.map Map.keys) . listAgentStates
      listAgentStates = fetchAgents [] <=< fmap Map.assocs . listControllers . rootController
        where  fetchAgents acc [] = return $ Map.fromList acc
               fetchAgents acc ((r, SomeController ctrl) : ctrls) =
                 case cast ctrl
                  of  Just (leaf :: AgentsManager res) ->
                        do  ags <- controlledAgents leaf
                            fetchAgents ((anyRole r, ags):acc) ctrls

      startAgents  = agentsMsgCtrl StartMessage  _agentSysExecBegan
      stopAgents   = agentsMsgCtrl StopMessage   _agentSysExecEnded

      agentsStarted  = atomically . tryReadTMVar . _agentSysExecBegan
      agentsStopped  = atomically . tryReadTMVar . _agentSysExecEnded

      tryGetResult  = atomically . tryReadTMVar . _agentSysExecResult
      waitResult    = atomically . readTMVar . _agentSysExecResult

      createAgentSystem debug = do  began   <- newEmptyTMVarIO
                                    ended   <- newEmptyTMVarIO
                                    result  <- newEmptyTMVarIO
                                    aDescr  <- defaultOverseerAgentDescriptor
                                                 "/" debug
                                    let  rootDescriptor =
                                            OverseerDescriptor
                                              (SomeRole RootRole)
                                              aDescr

                                         root = undefined
                                    return $ AgentsSysCtrl  began
                                                            ended
                                                            result
                                                            debug
                                                            root

  crAg :: ( Typeable r, Typeable states, Typeable res
          , Typeable (RoleStates r) ) =>
           r -> AgentDescriptor states res
             -> (RoleStates r -> AgentStatus' res)
             -> CreateAgent (AgentRunOfRole r) res
  crAg _ descr exState' = CreateAgent descr exState
    where exState = exState' . fromJust . extractAgentStates

  sendAllCtrls msg  =    (mapM_ (`send` msg) . Map.elems )
                    <=<  listControllers . rootController

  agentsMsgCtrl msg tvf c = do  let tv = tvf c
                                b <- atomically $ isEmptyTMVar tv
                                when b $ do  t <- getCurrentTime
                                             atomically $ putTMVar tv t
                                             sendAllCtrls msg c

\end{code}










%if standalone
\end{document}
%endif

%%% Local Variables:
%%% latex-build-command: "lhsTeX"
%%% lhs-build-standalone-flag: t
%%% eval: (haskell-indentation-mode)
%%% eval: (interactive-haskell-mode)
%%% End:
