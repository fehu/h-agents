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

module Agent.AgentSystem.Controller.AgentStatus (

  AgentStatus(..), AgentStatus', AgentWithStatus, AgentWithStatus'

, isWaiting, isLocked, getLocked, isTerminated, mbTerminated


) where

  import Control.Monad
  import Control.Exception (SomeException)
  import Control.Concurrent.STM

  import Agent.Abstract (AgentFullRef)

  -- _local_DEBUG = True
  -- localDebug scope = when _local_DEBUG . putStrLn . (("[DEBUG] " ++ scope ++ ": ") ++)

\end{code}
%endif



\verb|AgentStatus| represents the state of agent's \emph{execution} and should be
read directly from agent's internal state.


\begin{code}

  data AgentStatus res  =  Initialized
                        |  Negotiating
                        |  Waiting res
                        |  Locked  res
                        |  Terminated SomeException deriving Show

  isWaiting (Waiting _)  = True
  isWaiting _            = False

  isLocked (Locked _)  = True
  isLocked _           = False

  getLocked (Locked l) = l

  isTerminated (Terminated _)  = True
  isTerminated _               = False

  mbTerminated (Terminated ex)  = Just ex
  mbTerminated _                = Nothing


  type AgentStatus' res = TVar (AgentStatus res)

  type AgentWithStatus  res = (AgentFullRef, STM (AgentStatus res))
  type AgentWithStatus' res = (AgentFullRef, TVar (AgentStatus res))

\end{code}





%if standalone
\end{document}
%endif
