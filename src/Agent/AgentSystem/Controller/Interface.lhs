
%if False
\begin{code}

{-# LANGUAGE ExistentialQuantification
           , MultiParamTypeClasses
           , FunctionalDependencies
           , FlexibleContexts
           , Rank2Types
           , TypeFamilies
           , FlexibleInstances
         #-}

module Agent.AgentSystem.Controller.Interface (

  Controller(..), SomeController(..)
, ControllerLeaf(..), ControllerNode(..)

, AgentStatusMap, AgentsStatuses
, AgentsExecutionResult

, ControllerNotification(..)
, ListAgents(..), AgentsList(..)
, CreateAgent(..), CreateAgent'(..)

) where

  import Control.Monad
  import Control.Concurrent.STM
  import Control.Exception (SomeException)
  import Control.Arrow (second)

  import Data.Typeable
  import Data.Map (Map)
  import Data.Function (on)

  import qualified Data.Map as Map

  import Agent
  import Agent.AgentSystem.Controller.AgentStatus


  -- _local_DEBUG = True
  -- localDebug scope = when _local_DEBUG . putStrLn . (("[DEBUG] " ++ scope ++ ": ") ++)

\end{code}
%endif

\emph{Controller} type class provides an interface, independent of controller's
actual position in the hierarchy.

> class (AgentCommPriority c) => Controller c res | c -> res where

The following functions denote this kind of behavior:
\begin{itemize}
  \item Enumerates the controlled agents.

>   controlledAgents :: c -> IO (AgentStatusMap res)

  \item Gathers the \emph{results} or \emph{errors} of agents executions (if any).

>   tryGetResults :: c -> IO (Maybe (AgentsExecutionResult res))

\item Waits for \emph{all} agents to report the result or throw an exception.

>   waitResults :: c -> IO (AgentsExecutionResult res)

\item Terminates and unregisters given agent.

>   stopControlledAgent :: c -> AgentFullRef -> IO ()

\item Terminates the controller(s) and the controlled agents.

>   stopController :: c -> IO ()

\item May have a parent controller (except for the \emph{root} controller).

>   parentController  :: c -> Maybe (SomeController res)


> type AgentStatusMap  res = Map AgentFullRef (STM (AgentStatus res))
> type AgentsStatuses  res = Map AgentFullRef (AgentStatus res)

\end{itemize}

Execution result is represented as a \verb|Map|, with keys $\in$ \verb|AgentFullRef|
and values being \emph{either} agent's result or some exception,
thrown by the agent.

> type AgentsExecutionResult res = Map AgentFullRef (Either SomeException res)

\medskip

\noindent
The controller hierarchy is a \emph{tree}, where the \emph{nodes} control other
controllers and the \emph{leafs} handle agents. An example can be seen in figure
\ref{fig:controllerHierarchy}.


A \verb|ControllerLeaf| creates new agents and monitors their lifetime.

> class (Controller c res) => ControllerLeaf c res | c -> res
>   where
>     newAgentsL :: (Typeable ag)  => c
>                                  -> [CreateAgent ag res]
>                                  -> IO (AgentsStatuses res)

\verb|CreateAgent| extends \verb|AgentDescriptor|, adding a function to
extract \verb|AgentStatus'|.

% > data CreateAgent res = forall states ag . ( AgentCreate (AgentDescriptor states res) ag
% >                                           , Typeable ag, Typeable states )  =>
% >   CreateAgent {
% >     crAgDescriptor  :: (Typeable states) => AgentDescriptor states res,
% >     crAgExtState    :: ag -> AgentStatus' res
% >     }

> data CreateAgent ag res = forall states . ( AgentCreate (AgentDescriptor states res) ag
>                                           , Typeable states )  =>
>   CreateAgent {
>     crAgDescriptor  :: (Typeable states) => AgentDescriptor states res,
>     crAgExtState    :: ag -> AgentStatus' res
>     }

> data CreateAgent' = forall ag res . (Typeable res, Typeable ag) =>
>      CreateAgent' (CreateAgent ag res)

> instance Show (CreateAgent ag res) where
>   show CreateAgent{crAgDescriptor=d} = show d
> instance Show CreateAgent' where show (CreateAgent' c) = show c

A \verb|ControllerNode| controls other controllers, both nodes and leafs.
Agents creation by a \emph{node} controller may be ambiguous in case of a non-trivial
controllers hierarchy. It is not clear which of the underlying controllers
must actually handle the creation, therefore some hierarchy position
identifier is needed.

> class (Controller c res) => ControllerNode c res | c -> res
>   where
>     type AgentPosition c :: *
>     listControllers :: c -> IO (Map (AgentPosition c) (SomeController res))
> --    controllerPosition :: c -> AgentPosition c
>
>     newAgentsN :: (Typeable ag)  => c
>                                  -> AgentPosition c
>                                  -> [CreateAgent ag res]
>                                  -> IO [AgentWithStatus res]


\begin{figure}
  \label{fig:controllerHierarchy}
  \centering
  \begin{tikzpicture}[
    level 1/.style={sibling distance=10em},
    level 2/.style={sibling distance=6em},
    level 3/.style={sibling distance=2em},
    every node/.style = {align=center},
    ctrl/.style = {draw, shape=rectangle, rounded corners, minimum size=2.2em},
    leaf/.style = {fill=black!20},
    agent/.style = {draw, shape=circle, inner sep=1pt}
    ]
    \node[ctrl, label={root}] {$C_0$}
      child { node[ctrl] {$C_1^1$}
              child {  node[ctrl, leaf] {$C_2^1$}
                       child { node[agent] {$A_1^1$}}
                       child { node[agent] {$A_2^1$}}
                       child { node {$\dots$}}
                    }
              child {  node[ctrl, leaf] {$C_2^2$}
                       child { node[agent] {$A_1^2$}}
                       child { node[agent] {$A_2^2$}}
                       child { node {$\dots$}}
                    }
            }
      child { node[ctrl] { $C_1^2$ }
              child {  node[ctrl, leaf] {$C_2^3$}
                       child { node {$\dots$}}
                       child { node[agent] {$A_*^3$}}
                       child { node {$\dots$}}
                    }
              child {  node[ctrl, leaf] {$C_2^4$}
                       child { node {$\dots$}}
                       child { node[agent] {$A_*^4$}}
                       child { node {$\dots$}}
                    }
            }
      child { node[ctrl] { $C_1^3$ }
              child {  node[ctrl, leaf] {$C_2^5$}
                       child { node {$\dots$}}
                       child { node[agent] {$A_*^5$}}
                       child { node {$\dots$}}
                    }
              child {  node[ctrl, leaf] {$C_2^6$}
                       child { node[agent] {$A_1^6$}}
                       child { node[agent] {$A_2^6$}}
                       child { node {$\dots$}}
                    }
            };
  \end{tikzpicture}
  \caption{Controller hierarchy example.}
\end{figure}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\bigskip

\noindent
\verb|SomeController| container may wrap any type of class \verb|Controller|.
\verb|SomeController| has itself an instance of \verb|Controller|.

> data SomeController res = forall c . ( Controller c res
>                                  --    , Show (AgentPosition c)
>                                  --    , Typeable (AgentPosition c)
>                                      ) =>
>      SomeController c
>   deriving Typeable

> someControllerId (SomeController c) = agentId c

\begin{code}
  instance Eq  (SomeController res) where (==) = (==) `on` someControllerId
  instance Ord (SomeController res) where compare = compare `on` someControllerId

  instance (Typeable res) => AgentComm (SomeController res) where
    agentId = someControllerId
    send (SomeController c) = send c
    ask  (SomeController c) = ask c

  instance (Typeable res) => AgentCommPriority (SomeController res) where
    sendPriority (SomeController c) = sendPriority c
    askPriority  (SomeController c) = askPriority c

  instance (Typeable res) => Controller (SomeController res) res where
    controlledAgents (SomeController c) = controlledAgents c
    tryGetResults (SomeController c)    = tryGetResults c
    waitResults (SomeController c)      = waitResults c
    stopControlledAgent (SomeController c) = stopControlledAgent c
    stopController (SomeController c)   = stopController c
    parentController (SomeController c) = parentController c
\end{code}



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\bigskip


Notifications and other messages:

\begin{code}
  data ControllerNotification  = TerminatedByRequest
                               | HaveAllResults
                               | AgentsFailed (Map AgentFullRef SomeException)
    deriving (Show, Typeable)

 --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

  data ListAgents = ListAgents deriving (Eq, Show, Typeable)
  type instance ExpectedResponse ListAgents = AgentsList

  data AgentsList = forall res . (Typeable res) => AgentsList (AgentsStatuses res)
    deriving Typeable

  -- unwrapAgentsList (AgentsList l) = l
  agentsKeys (AgentsList l) = Map.keys l

  instance Eq AgentsList where (==) = (==) `on` agentsKeys
  instance Show AgentsList where show = show . agentsKeys


  type instance ExpectedResponse [CreateAgent'] = AgentsList

\end{code}



% >  newAgents :: forall states ag s . ( Typeable ag, Typeable s, Show s
% >                                    , AgentCreate (AgentDescriptor states res) ag )
% >             => c  -> [CreateAgent states res ag s]
% >                   -> IO [AgentWithStatus res]
% >  controlledAgents :: c -> IO [AgentWithStatus res]
% >  tryGetResult :: c -> IO (Maybe (Either SomeException res))
% >  waitResult   :: c -> IO (Either SomeException res)
% >  terminateController :: c -> IO ()
%
% >  parentController  :: c -> Maybe (SomeController res)
% >  notifyParentCtrl  :: forall msg . Message msg => c -> msg -> IO ()
%
% > data SomeController res = forall c . Controller c res => SomeController c
%
% > data CreateAgent states res ag s = CreateAgent{
% >   crAgDescriptor  :: AgentDescriptor states res,
% >   crAgExtState    :: ag -> s
% >   }
