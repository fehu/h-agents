-----------------------------------------------------------------------------
--
-- Module      :  AgentsManager.Manager
-- License     :  MIT
--
-- Maintainer  :  kdn.kovalev@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable (requires STM)
--
-----------------------------------------------------------------------------

{-# LANGUAGE ExistentialQuantification #-}

module AgentSystem.Manager(

  AgentsManager(..)
, AgentsCreation(..)

, SimpleAgentsManager, newSimpleAgentsManager

) where

import Agent

import Control.Monad ( (<=<) )

import Data.Map (Map)
import qualified Data.Map as Map

import Control.Concurrent.STM

-----------------------------------------------------------------------------

class AgentsManager m where
    listAgents :: m -> IO [SomeAgentRef]
    findAgent  :: m -> String -> IO (Maybe SomeAgentRef)

    startAllAgents      :: m -> IO ()
    pauseAllAgents      :: m -> IO ()
    terminateAllAgents  :: m -> IO ()
    waitAllAgents       :: m -> IO ()

    startAllAgents      = mapM_ agentStart           <=< listAgents
    pauseAllAgents      = mapM_ agentPause           <=< listAgents
    terminateAllAgents  = mapM_ agentTerminate       <=< listAgents
    waitAllAgents       = mapM_ agentWaitTermination <=< listAgents


class (AgentsManager m) => AgentsCreation m where
  newAgent  :: forall from res . CreateAgentRef from res =>
                m -> from -> IO (AgentRef res)

-----------------------------------------------------------------------------

newSimpleAgentsManager = SimpleAgentsManager <$> newTVarIO Map.empty

data SimpleAgentsManager = SimpleAgentsManager{
  _simpleAgentsRegister :: TVar (Map String SomeAgentRef)
  }

-----------------------------------------------------------------------------

getSimpleRegister = readTVarIO . _simpleAgentsRegister

instance AgentsManager SimpleAgentsManager where
  listAgents = fmap Map.elems . getSimpleRegister
  findAgent sys aId = Map.lookup aId <$> getSimpleRegister sys

instance AgentsCreation SimpleAgentsManager where
  newAgent sys from  =
    do ref <- createAgentRef from
       atomically $ do
         let var = _simpleAgentsRegister sys
             k = agentId ref
         m <- readTVar var
         if Map.notMember k m
           then var `writeTVar` Map.insert k (someAgentRef ref) m
           else fail $ "Agent with id '" ++ show k
                    ++ "' already exists in the system."
       return ref

-----------------------------------------------------------------------------
