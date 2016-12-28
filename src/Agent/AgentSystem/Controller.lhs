%if standalone
\documentclass{article}

% include src/Document/format.fmt
%include polycode.fmt
%include forall.fmt

\usepackage[english]{babel}
\usepackage[inline, shortlabels]{enumitem}

\usepackage{tikz}
\usetikzlibrary{trees}

\usepackage{showframe}

\begin{document}
%endif

%if False
\begin{code}

module Agent.AgentSystem.Controller (module Export ) where

  import Control.Monad
  import Data.Typeable


  import Agent                                    as Export
  import Agent.AgentSystem.Controller.Interface   as Export
  import Agent.AgentSystem.Controller.AgentStatus as Export
  import Agent.AgentSystem.Controller.Leaf        as Export
  import Agent.AgentSystem.Controller.Node        as Export


  -- _local_DEBUG = True
  -- localDebug scope = when _local_DEBUG . putStrLn . (("[DEBUG] " ++ scope ++ ": ") ++)

\end{code}
%endif

The negotiation is created and monitored by the \emph{controller(s)}, that
may be composed into hierarchical structure.

%include Agent/AgentSystem/Controller/Interface.lhs
%include Agent/AgentSystem/Controller/AgentStatus.lhs
%include Agent/AgentSystem/Controller/Leaf.lhs
%include Agent/AgentSystem/Controller/Node.lhs



%if standalone
\end{document}
%endif


%%% Local Variables:
%%% latex-build-command: "lhsTeX"
%%% lhs-copy-literate-src-flag: t
%%% lhs-build-standalone-flag: t
%%% End:
