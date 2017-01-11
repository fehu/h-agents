-----------------------------------------------------------------------------
--
-- Module      :  AgentRoleSystem
-- License     :  MIT
--
-- Maintainer  :  kdn.kovalev@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable (requires STM)
--
-----------------------------------------------------------------------------


{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

module AgentRoleSystem (

  AgentSystemRoles(..)
, AgentsOfRole(..), AgentMaybeResultsOfRole(..), AgentResultsOfRole(..)

, RoleAgentSystem, newRoleAgentSystem

, module AgentRole
, AgentSystem(..)

) where

import AgentRole
import AgentSystem

import Data.Typeable

import Data.Map (Map)
import qualified Data.Map  as Map
import qualified Data.List as List


import Control.Applicative ( (<|>) )
import Control.Monad ( (<=<), forM )
import Control.Concurrent.STM

-----------------------------------------------------------------------------

data AgentsOfRole = forall r . AgentRole' r =>
    AgentsOfRole r [AgentRef (RoleResult r)]

data AgentMaybeResultsOfRole = forall r . AgentRole' r =>
    AgentMaybeResultsOfRole r [(String, Maybe (AgentExecutionResult (RoleResult r)))]

data AgentResultsOfRole = forall r . AgentRole' r =>
    AgentResultsOfRole r [(String, AgentExecutionResult (RoleResult r))]

type AgentRole' r = (AgentRole r, Typeable (RoleResult r), Show (RoleResult r))

class AgentSystemRoles sys where
  listAgentsByRole  :: sys -> IO [AgentsOfRole]
  listAgentsOfRole  :: (AgentRole' r) => sys -> r
                    -> IO [AgentRef (RoleResult r)]
  findAgentOfRole   :: (AgentRole' r) => sys -> r -> String
                    -> IO (Maybe (AgentRef (RoleResult r)))

  collectResult     :: (AgentRole' r) => sys -> r -> String
                    -> IO (Maybe (AgentExecutionResult (RoleResult r)))
  collectResults    :: (AgentRole' r) => sys -> r
                    -> IO [(String, Maybe (AgentExecutionResult (RoleResult r)))]
  collectAllResults :: sys -> IO [AgentMaybeResultsOfRole]

  awaitResult       :: (AgentRole' r) => sys -> r -> String
                    -> IO (AgentExecutionResult (RoleResult r))
  awaitResults      :: (AgentRole' r) => sys -> r
                    -> IO [(String, AgentExecutionResult (RoleResult r))]
  awaitAllResults   :: sys -> IO [AgentResultsOfRole]

  newAgentOfRole :: forall r . AgentRole' r =>
                    sys
                 -> AgentRoleDescriptor r
                 -> IO (RoleArgs r)
                 -> IO (AgentRef (RoleResult r))

  collectResult s r = maybe (return Nothing) agentResult <=< findAgentOfRole s r
  collectResults s  = mapM (agentIdM agentResult) <=< listAgentsOfRole s
  collectAllResults = agentsOfRoleResult <=< listAgentsByRole
    where agentsOfRoleResult []                          = return []
          agentsOfRoleResult (AgentsOfRole r refs : ars) =
            (:) <$> (AgentMaybeResultsOfRole r <$>
                      mapM (agentIdM agentResult) refs)
                <*> agentsOfRoleResult ars

  awaitResult s r i = maybe (fail $ notFound r i) agentWaitResult
                    =<< findAgentOfRole s r i
  awaitResults s    = mapM (agentIdM agentWaitResult) <=< listAgentsOfRole s
  awaitAllResults   = mapM waitResults <=< listAgentsByRole
    where waitResults (AgentsOfRole r refs) =
            AgentResultsOfRole r <$> mapM (agentIdM agentWaitResult) refs



agentIdM f ag = (,) <$> return (agentId ag) <*> f ag

notFound r i = "Agent of role '" ++ show (roleName r) ++
               "' with id <" ++ show i ++ "> couldn't be found."

printResults r l =
  let header  = "Results for role '" ++ roleName r ++ "':"
      results = do (k, res) <- l
                   ["  <" ++ k ++ "> " ++ show res]
  in unlines $ header : results

instance Show AgentMaybeResultsOfRole where
  show (AgentMaybeResultsOfRole r l) = printResults r l

instance Show AgentResultsOfRole where
  show (AgentResultsOfRole r l) = printResults r l

-----------------------------------------------------------------------------

newRoleAgentSystem = RoleAgentSystem <$> newTVarIO []

data RoleAgentSystem = RoleAgentSystem{
  _roleAgentsRegisters :: TVar [RoleRegister]
  }

data RoleRegister = forall r . AgentRole' r =>
    RoleRegister r (TVar (AgentsRoleMap r))

type AgentsRoleMap r = Map String (AgentRef (RoleResult r))


lookupRoleRegister :: (AgentRole r, Typeable (RoleResult r)) =>
                       r -> [RoleRegister] -> Maybe (TVar (AgentsRoleMap r))
lookupRoleRegister r = cast <=<
                       List.find (\(RoleRegister r' _) -> r' `isSameRole` r)

lookupAgentsOfRole :: (AgentRole r, Typeable (RoleResult r)) =>
                      [RoleRegister] -> r -> IO (Maybe (AgentsRoleMap r))
lookupAgentsOfRole rrs r = case lookupRoleRegister r rrs
                            of Just var -> cast <$> readTVarIO var
                               _        -> return Nothing

lookupAgentsOfRole' sys r = do regs <- readTVarIO $ _roleAgentsRegisters sys
                               lookupAgentsOfRole regs r


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



instance AgentSystemRoles RoleAgentSystem where
  listAgentsByRole = atomically
                   . (mkAgentsOfRole <=< readTVar . _roleAgentsRegisters)
      where
        mkAgentsOfRole [] = return []
        mkAgentsOfRole (RoleRegister r var : regs) =
          do rMap <- readTVar var
             (AgentsOfRole r (Map.elems rMap) :) <$> mkAgentsOfRole regs

  listAgentsOfRole s r = maybe [] Map.elems <$> lookupAgentsOfRole' s r

  findAgentOfRole sys r aId = do
    mbReg <- lookupAgentsOfRole' sys r
    return $ Map.lookup aId =<< mbReg

  newAgentOfRole s d argsIO = do
    ref <- createAgentRef $ CreateAgentOfRole d argsIO
    let (AgentRoleDescriptor r _) = d
        newRegister =
          do newMap <- newTVar $ Map.singleton (agentId ref) ref
             _roleAgentsRegisters s `modifyTVar` (RoleRegister r newMap :)

    atomically $ do regs <- readTVar $ _roleAgentsRegisters s
                    maybe newRegister
                          (`modifyTVar` Map.insert (agentId ref) ref)
                          (lookupRoleRegister r regs)
    return ref

-----------------------------------------------------------------------------
