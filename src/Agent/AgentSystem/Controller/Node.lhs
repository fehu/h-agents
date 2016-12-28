%if False
\begin{code}

{-# LANGUAGE ExistentialQuantification
           , ScopedTypeVariables
           , FlexibleInstances
           , MultiParamTypeClasses
           , TypeFamilies
        #-}

module Agent.AgentSystem.Controller.Node (

  AgentsOverseer(..), OverseerDescriptor(..), OverseerRole(..)
, createOverseer, CreateAgentsOverseer
, defaultOverseerAgentDescriptor                                      -- TODO

) where

  -- import Control.Monad
  -- import Control.Concurrent.STM
  import Control.Exception (SomeException)

  import Data.Typeable
  import Data.Function (on)
  import Data.Maybe (fromJust)

  import Agent
  -- import Agent.AgentSystem.Controller.AgentStatus
  import Agent.AgentSystem.Controller.Interface
  -- import Agent.AgentSystem.Controller.Leaf

  -- _local_DEBUG = True
  -- localDebug scope = when _local_DEBUG . putStrLn . (("[DEBUG] " ++ scope ++ ": ") ++)

\end{code}
%endif

\verb|ControllerNode| implementation --- \verb|AgentsOverseer|.

\begin{code}

  data AgentsOverseer hp res = AgentsOverseer  AgentFullRef
                                               hp
                                               (Maybe (SomeController res))
    deriving Typeable

  overseerRef (AgentsOverseer ref _ _) = ref

  instance Eq  (AgentsOverseer hp res) where (==) = (==) `on` overseerRef
  instance Ord (AgentsOverseer hp res) where compare = compare `on` overseerRef

  instance (Typeable res, Typeable hp) =>
    AgentComm (AgentsOverseer hp res) where
      agentId = agentId . overseerRef
      send = send . overseerRef
      ask = ask . overseerRef

  instance (Typeable res, Typeable hp) =>
    AgentCommPriority (AgentsOverseer hp res) where
      sendPriority = sendPriority . overseerRef
      askPriority = askPriority . overseerRef

  instance (Typeable res, Typeable hp) =>
    Controller (AgentsOverseer hp res) res where
      controlledAgents  = fetchAgents ListAgents
      tryGetResults     = fetchResults GetResults
      waitResults       = fetchResults WaitResults
      stopControlledAgent c = send c . StopAgent
      stopController = flip sendPriority StopMessage
      parentController (AgentsOverseer _ _  p) = p

  instance (Typeable res, Typeable hp, Show hp) =>
    ControllerNode (AgentsOverseer hp res) res where
      type AgentPosition (AgentsOverseer hp res) = hp

      listControllers c = do  AnyControllers cs <- c `ask` ListControllers
                              return . fromJust $ cast cs
      newAgentsN c hp cs = fetchAgents (hp, map CreateAgent' cs) c



  fetchAgents msg c = do  AgentsList ags <- c `ask` msg
                          return . fromJust $ cast ags
  fetchResults msg c = do  Results ags <- c `ask` msg
                           return . fromJust $ cast ags

\end{code}

Messages:

\begin{code}

  data GetResults = GetResults deriving (Eq, Show, Typeable)
  data WaitResults = WaitResults deriving (Eq, Show, Typeable)

  instance Eq SomeException where _ == _ = False

  data Results = forall res . (Typeable res, Eq res, Show res) =>
      Results (AgentsExecutionResult res)
   deriving Typeable

  instance Eq Results where
   (Results r1) == (Results r2') = maybe False (r1 ==) (cast r2')

  instance Show Results where show (Results r) = show r

  type instance ExpectedResponse GetResults   = Results
  type instance ExpectedResponse WaitResults  = Results

  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

  data StopAgent = StopAgent AgentFullRef  deriving (Eq, Show, Typeable)

 --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

  data ListControllers  = ListControllers deriving (Eq, Show, Typeable)
  data AnyControllers   = forall res .  Message res => AnyControllers [SomeController res]
    deriving Typeable

  instance Eq AnyControllers where
    (AnyControllers c1) == (AnyControllers c2) = maybe False (c1 ==) (cast c2)

  instance Show AnyControllers where
    show (AnyControllers cs) = show $ map agentId cs

  type instance ExpectedResponse ListControllers = AnyControllers

 --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

  type instance ExpectedResponse (hp, [CreateAgent']) = AgentsList

\end{code}


The \emph{node} is created from an \verb|OverseerDescriptor|:

\begin{code}

  data OverseerDescriptor hp s res = OverseerDescriptor{
    overseerPosition :: hp,
    overseerDescriptor :: AgentDescriptor s res
    }


  -- data OverseerDescriptor hp s res = OverseerDescriptor{
    -- overseerName :: String,
    -- overseerPosition :: hp,
    -- overseerDescriptor :: AgentDescriptor s ()
    -- overseerNoResult :: res
    -- }

  data OverseerRole = OverseerRole deriving (Typeable, Show)

  type CreateAgentsOverseer hp res  = Maybe (SomeController res)
                                    -> IO (AgentsOverseer hp res)

  createOverseer :: (Typeable s) =>
    OverseerDescriptor hp s res -> CreateAgentsOverseer hp res

  createOverseer d parent =
    do  (_ :: ExecutableAgent, ref)
           <- createAgent $ overseerDescriptor d
        return $ AgentsOverseer ref (overseerPosition d) parent

\end{code}

Default overseer descriptor \red{TODO}:

\begin{code}

  defaultOverseerAgentDescriptor :: String -> Bool -> IO (AgentDescriptor () ())
  defaultOverseerAgentDescriptor name =
    newAgentDescriptor name behavior (return ()) ()
      where  behavior = AgentBehavior
                          AgentNoAct
                        $ AgentHandleMessages
                            handleMessage'
                            respondMessage'
             handleMessage' = undefined
             respondMessage' = undefined


\end{code}
