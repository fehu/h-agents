-----------------------------------------------------------------------------
--
-- Module      :  AgentSystem
-- License     :  MIT
--
-- Maintainer  :  kdn.kovalev@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable (requires STM)
--
-----------------------------------------------------------------------------

{-# LANGUAGE ExistentialQuantification #-}

module AgentSystem (

  AgentSystem(..)

, SimpleAgentSystem

, module Agent

) where

import Agent

import Data.Map (Map)
import qualified Data.Map  as Map

import Control.Monad ( (<=<), when )
import Control.Concurrent.STM

-----------------------------------------------------------------------------

class AgentSystem sys where
    newAgentSystem :: IO sys

    newAgent  :: forall from res . CreateAgentRef from res =>
                  sys -> from -> IO (AgentRef res)

    listAgents :: sys -> IO [SomeAgentRef]
    findAgent  :: sys -> String -> IO (Maybe SomeAgentRef)

    startAllAgents      :: sys -> IO ()
    pauseAllAgents      :: sys -> IO ()
    terminateAllAgents  :: sys -> IO ()
    waitAllAgents       :: sys -> IO ()

    startAllAgents      = mapM_ agentStart           <=< listAgents
    pauseAllAgents      = mapM_ agentPause           <=< listAgents
    terminateAllAgents  = mapM_ agentTerminate       <=< listAgents
    waitAllAgents       = mapM_ agentWaitTermination <=< listAgents

-----------------------------------------------------------------------------

data SimpleAgentSystem = SimpleAgentSystem{
  _simpleAgentsRegister :: TVar (Map String SomeAgentRef)
  }

getSimpleRegister = readTVarIO . _simpleAgentsRegister

instance AgentSystem SimpleAgentSystem where
  newAgentSystem = SimpleAgentSystem <$> newTVarIO Map.empty

  listAgents = fmap Map.elems . getSimpleRegister
  findAgent sys aId = Map.lookup aId <$> getSimpleRegister sys

  newAgent sys from   = do ref <- createAgentRef from
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
