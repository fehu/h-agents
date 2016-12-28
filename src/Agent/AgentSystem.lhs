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


   module Export

) where

  import Data.Typeable

  import Agent.AgentSystem.Controller as Export

  -- _local_DEBUG = True
  -- localDebug scope = when _local_DEBUG . putStrLn . (("[DEBUG] " ++ scope ++ ": ") ++)

\end{code}
%endif


An \emph{agent system} uses agent \emph{roles} to create control hierarchy.
Furthermore, roles serve as agents \emph{archetypes}, defining agents' states
structure and behavior traits.
% Furthermore, roles define agent \emph{archetypes}, that determine how
% states and behavior are generated, given some role specific inputs.

> class (Show r, Typeable r, Typeable (RoleStates r)) =>
>   AgentRoleDescriptor r res | r -> res where

Roles of different types should still be comparable.

>     roleIx :: r -> Int

Each role has some specific \emph{input}.
% that is used to create initial states
% and defined agent's behavior (within the archetype).

>     type RoleInput  r :: *

The \emph{input} is used to create agent's \emph{states}.

>     type RoleStates r :: *
>     roleStates     :: r -> RoleInput r   -> IO (RoleStates r)
>     roleStatesExt  :: r -> RoleStates r  -> AgentStatus' res

The \emph{input} may be used to define agent's behavior.

>     roleBehavior   :: r -> RoleInput r   -> AgentBehavior (RoleStates r)

Agent's ID is depends on the \emph{role}.

>     nextRoleId     :: r -> IO AgentId

\bigskip

\noindent
Agent system should:

> class AgentSystem sys where

\begin{enumerate}
  \item be created from \red{?};

>   createAgentSystem :: ?? -> sys

\end{enumerate}



%if standalone
\end{document}
%endif

%%% Local Variables:
%%% latex-build-command: "lhsTeX"
%%% lhs-build-standalone-flag: t
%%% eval: (haskell-indentation-mode)
%%% eval: (interactive-haskell-mode)
%%% End:
