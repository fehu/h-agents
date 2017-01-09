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
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module AgentSystem (

) where

import AgentRole

import Data.Typeable
import Data.Map (Map)
import qualified Data.Map  as Map
import qualified Data.List as List

import Control.Applicative ( (<|>) )
import Control.Monad ( (<=<), forM )
import Control.Concurrent.STM

-----------------------------------------------------------------------------

newtype AgentId = AgentId String deriving (Show, Eq, Ord)

agentId :: (AgentControl a) => a -> AgentId
agentId = AgentId . agentName

-----------------------------------------------------------------------------

class AgentSystem sys where
    newAgent  :: forall from res . CreateAgentRef from res =>
                  sys -> from -> IO (AgentRef res)

    listAgents :: sys -> IO [SomeAgentRef]
    findAgent  :: sys -> AgentId -> IO (Maybe SomeAgentRef)

    startAllAgents      :: sys -> IO ()
    pauseAllAgents      :: sys -> IO ()
    terminateAllAgents  :: sys -> IO ()
    waitAllAgents       :: sys -> IO ()

    startAllAgents      = mapM_ agentStart           <=< listAgents
    pauseAllAgents      = mapM_ agentPause           <=< listAgents
    terminateAllAgents  = mapM_ agentTerminate       <=< listAgents
    waitAllAgents       = mapM_ agentWaitTermination <=< listAgents

-----------------------------------------------------------------------------

data AgentsOfRole = forall r . AgentRole r =>
    AgentsOfRole r [AgentRef (RoleResult r)]

data AgentMaybeResultsOfRole = forall r . AgentRole r =>
    AgentMaybeResultsOfRole r [(AgentId, Maybe (AgentExecutionResult (RoleResult r)))]

data AgentResultsOfRole = forall r . AgentRole r =>
    AgentResultsOfRole r [(AgentId, AgentExecutionResult (RoleResult r))]

class (AgentSystem sys) => AgentSystemRoles sys where
  listAgentsByRole  :: sys -> IO [AgentsOfRole]
  findAgentOfRole   :: (AgentRole r) => sys -> r -> AgentId -> AgentRef (RoleResult r)

  collectResult     :: (AgentRole r) => sys -> r -> AgentId
                    -> IO (Maybe (AgentExecutionResult (RoleResult r)))
  collectResults    :: (AgentRole r) => sys -> r
                    -> IO [(AgentId, Maybe (AgentExecutionResult (RoleResult r)))]
  collectAllResults :: sys -> IO [AgentMaybeResultsOfRole]

  awaitResult       :: (AgentRole r) => sys -> r -> AgentId
                    -> IO (AgentExecutionResult (RoleResult r))
  awaitResults      :: (AgentRole r) => sys -> r
                    -> IO [(AgentId, AgentExecutionResult (RoleResult r))]
  awaitAllResults   :: sys -> IO [AgentResultsOfRole]

  newAgentOfRole :: forall r . AgentRole r =>
                    sys
                 -> AgentRoleDescriptor r
                 -> IO (RoleArgs r)
                 -> IO (AgentRef (RoleResult r))

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

data SimpleAgentSystem = SimpleAgentSystem{
  _simpleAgentsRegister :: TVar (Map AgentId SomeAgentRef)
  }

getSimpleRegister = readTVarIO . _simpleAgentsRegister

instance AgentSystem SimpleAgentSystem where
  listAgents = fmap Map.elems . getSimpleRegister
  findAgent sys aId = Map.lookup aId <$> getSimpleRegister sys

  newAgent sys from   = do ref <- createAgentRef from
                           atomically . modifyTVar' (_simpleAgentsRegister sys)
                                      $ Map.insert (agentId ref) (someAgentRef ref)
                           return ref

-----------------------------------------------------------------------------

type AgentsRoleMap r = Map AgentId (AgentRef (RoleResult r))

data RoleRegister = forall r . (AgentRole r, Typeable (RoleResult r)) =>
    RoleRegister r (TVar (AgentsRoleMap r))

lookupAgentsOfRole :: (AgentRole r, Typeable (RoleResult r)) =>
                      [RoleRegister] -> r -> IO (Maybe (AgentsRoleMap r))
lookupAgentsOfRole rrs r = let mb = List.find (\(RoleRegister r' _) -> r' `isSameRole` r) rrs
                           in case mb of Just (RoleRegister _ var) -> cast <$> readTVarIO var
                                         _                         -> return Nothing

data RoleAgentSystem = RoleAgentSystem{
  _roleAgentsRegisters :: TVar [RoleRegister]
  }

instance AgentSystem RoleAgentSystem where
  listAgents sys = do regs <- readTVarIO $ _roleAgentsRegisters sys
                      maps <- forM regs $
                        \(RoleRegister _ var) ->
                          Map.map someAgentRef <$> readTVarIO var
                      return $ concatMap Map.elems maps
  findAgent sys aId = findAgent' aId =<< readTVarIO (_roleAgentsRegisters sys)

  newAgent sys from = fail "Use `newAgentOfRole` instead of `newAgent`"

findAgent' _ []                          = return Nothing
findAgent' k (RoleRegister _ var : regs) =
  do agMap <- readTVarIO var
     maybe (findAgent' k regs)
           (return . Just)
           (someAgentRef <$> Map.lookup k agMap)

-- data GenericAgentSystem = GenericAgentSystem{
    -- agentsRegister :: TVar (Map SomeRole (Map AgentId AgentRef))
  -- }

-- instance AgentSystem GenericAgentSystem where
--   newAgent sys from =
