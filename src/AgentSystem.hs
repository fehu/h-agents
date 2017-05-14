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
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}


module AgentSystem(

  AgentSystem(..)
, SystemAgentsCreation(..), SystemArgsProvider(..)

, AgentRole', AgentsOfRoles
, AgentResultsOfRoles, AgentMaybeResultsOfRoles
, SomeAgentSystem(..), KnownAgentSystem(..)

, SimpleAgentSystem, newSimpleAgentSystem, changeSimpleAgentSystemArgs

, module Export

) where

import AgentSystem.Manager as Export
import AgentSystem.Role    as Export

import Data.Maybe (fromMaybe)
import Data.Typeable (Typeable, cast)
import qualified Data.List as List

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set


import Control.Monad ( (<=<), forM )

import Control.Concurrent.STM

-----------------------------------------------------------------------------

class (AgentsManager sys) => AgentSystem sys where

  listAgentsByRole  :: sys -> IO AgentsOfRoles
  listAgentsOfRole  :: (AgentRole' r) => sys -> r
                    -> IO [AgentRef (RoleResult r)]
  listAgentsOfRoles :: (AgentRole' r) => sys -> [r]
                    -> IO [AgentRef (RoleResult r)]
  findAgentOfRole   :: (AgentRole' r) => sys -> r -> AgentId
                    -> IO (Maybe (AgentRef (RoleResult r)))

  collectResult     :: (AgentRole' r) => sys -> r -> AgentId
                    -> IO (Maybe (AgentExecutionResult (RoleResult r)))
  collectResults    :: (AgentRole' r) => sys -> r
                    -> IO [(AgentRefOfRole r, Maybe (AgentExecutionResult (RoleResult r)))]
  collectAllResults :: sys -> IO AgentMaybeResultsOfRoles

  awaitResult       :: (AgentRole' r) => sys -> r -> AgentId
                    -> IO (AgentExecutionResult (RoleResult r))
  awaitResults      :: (AgentRole' r) => sys -> r
                    -> IO [(AgentRefOfRole r, AgentExecutionResult (RoleResult r))]
  awaitAllResults   :: sys -> IO AgentResultsOfRoles

  listAgentsOfRole sys = listAgentsOfRoles sys . (: [])

  collectResult s r = maybe (return Nothing) agentResult <=< findAgentOfRole s r
  collectResults s  = mapM (agentM agentResult) <=< listAgentsOfRole s
  collectAllResults = mapM' agentsOfRoleResult <=< listAgentsByRole
    where agentsOfRoleResult (AgentsOfRole r refs) =
            AgentMaybeResultsOfRole r <$> mapM (agentM agentResult) refs

  awaitResult s r i = maybe (fail $ notFound r i) agentWaitResult
                    =<< findAgentOfRole s r i
  awaitResults s    = mapM (agentM agentWaitResult) <=< listAgentsOfRole s
  awaitAllResults   = mapM' waitResults <=< listAgentsByRole
    where waitResults (AgentsOfRole r refs) =
            AgentResultsOfRole r <$> mapM (agentM agentWaitResult) refs


agentM f ag = (,) <$> return ag <*> f ag

mapM' :: (Monad m, Ord k) => (a -> m b) -> Map k a -> m (Map k b)
mapM' f m = Map.fromList <$> mapM (\(k,v) -> (,) k <$> f v ) (Map.assocs m)

notFound r i = "Agent of role '" ++ show (roleName r) ++
               "' with id <" ++ show i ++ "> couldn't be found."

-----------------------------------------------------------------------------

class (AgentSystem sys) => SystemArgsProvider sys sysArgs where
  agentSysArgs :: sys -> IO sysArgs

class (SystemArgsProvider sys sysArgs) =>
  SystemAgentsCreation sys sysArgs where
    newAgentOfRole :: forall r ag . ( AgentRole' r, AgentOfRole ag r
                                    , RoleSysArgs r ~ sysArgs
                                    ) =>
                      sys
                   -> AgentRoleDescriptor r ag
                   -> IO (RoleArgs r)
                   -> IO (AgentRef (RoleResult r))

-----------------------------------------------------------------------------

instance (AgentSystem sys) =>
  SystemArgsProvider sys () where agentSysArgs = const $ return ()

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

type AgentsOfRoles = Map SomeRole AgentsOfRole
data AgentsOfRole = forall r . AgentRole' r =>
    AgentsOfRole r [AgentRef (RoleResult r)]

type AgentRole' r = (AgentRole r, Typeable (RoleResult r), Show (RoleResult r))

-----------------------------------------------------------------------------

type AgentResultsOfRoles = Map SomeRole AgentResultsOfRole
data AgentResultsOfRole = forall r . AgentRole' r =>
    AgentResultsOfRole r [(AgentRefOfRole r, AgentExecutionResult (RoleResult r))]

-----------------------------------------------------------------------------

type AgentMaybeResultsOfRoles = Map SomeRole AgentMaybeResultsOfRole
data AgentMaybeResultsOfRole = forall r . AgentRole' r =>
    AgentMaybeResultsOfRole r [(AgentRefOfRole r, Maybe (AgentExecutionResult (RoleResult r)))]

-----------------------------------------------------------------------------

printResults r l =
  let header  = "Results for role '" ++ roleName r ++ "':"
      results = do (k, res) <- l
                   ["  <" ++ rawAgentId (agentId k) ++ "> " ++ show res]
  in unlines $ header : results

instance Show AgentMaybeResultsOfRole where
  show (AgentMaybeResultsOfRole r l) = printResults r l

instance Show AgentResultsOfRole where
  show (AgentResultsOfRole r l) = printResults r l

-----------------------------------------------------------------------------

data SomeAgentSystem = forall sys . AgentSystem sys =>
     SomeAgentSystem sys

instance AgentsManager SomeAgentSystem where
  listAgents (SomeAgentSystem sys) = listAgents sys
  findAgent  (SomeAgentSystem sys) = findAgent sys


instance AgentSystem SomeAgentSystem where
  listAgentsByRole  (SomeAgentSystem sys) = listAgentsByRole  sys
  listAgentsOfRoles (SomeAgentSystem sys) = listAgentsOfRoles sys
  findAgentOfRole   (SomeAgentSystem sys) = findAgentOfRole   sys


-----------------------------------------------------------------------------

data KnownAgentSystem sysArgs = forall sys . SystemAgentsCreation sys sysArgs =>
     KnownAgentSystem sys

instance AgentsManager (KnownAgentSystem args) where
  listAgents (KnownAgentSystem sys) = listAgents sys
  findAgent  (KnownAgentSystem sys) = findAgent sys

instance AgentSystem (KnownAgentSystem args) where
  listAgentsByRole  (KnownAgentSystem sys) = listAgentsByRole  sys
  listAgentsOfRoles (KnownAgentSystem sys) = listAgentsOfRoles sys
  findAgentOfRole   (KnownAgentSystem sys) = findAgentOfRole   sys

instance SystemArgsProvider (KnownAgentSystem args) args where
  agentSysArgs   (KnownAgentSystem sys) = agentSysArgs sys

instance SystemAgentsCreation (KnownAgentSystem args) args where
  newAgentOfRole (KnownAgentSystem sys) = newAgentOfRole sys

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

data SimpleAgentSystem sysArgs = SimpleAgentSystem{
    _agentsRegisters :: TVar [RoleRegister]
  , _agentSysArgs    :: IO sysArgs
  }

newSimpleAgentSystem sysArgs = flip SimpleAgentSystem sysArgs <$> newTVarIO []

instance Functor SimpleAgentSystem where
  fmap f (SimpleAgentSystem reg args) = SimpleAgentSystem reg $ f <$> args

changeSimpleAgentSystemArgs :: (sysArgs0 -> IO sysArgs) -> SimpleAgentSystem sysArgs0
                                                        -> SimpleAgentSystem sysArgs
changeSimpleAgentSystemArgs f (SimpleAgentSystem reg args) =
    SimpleAgentSystem reg $ f =<< args

-----------------------------------------------------------------------------

data RoleRegister = forall r . AgentRole' r =>
    RoleRegister r (TVar (AgentsRoleMap r))

type AgentsRoleMap r = Map AgentId (AgentRef (RoleResult r))

-----------------------------------------------------------------------------

lookupRoleRegister :: (AgentRole r, Typeable (RoleResult r)) =>
                       r -> [RoleRegister] -> Maybe (TVar (AgentsRoleMap r))
lookupRoleRegister r = cast <=<
                       List.find (\(RoleRegister r' _) -> r' `roleEq` r)


searchAgentsOfRoles :: (AgentRole r, Typeable (RoleResult r)) =>
                      [RoleRegister] -> [r] -> IO (AgentsRoleMap r)
searchAgentsOfRoles rrs rs = fmap Map.unions . sequence
  $ do r <- rs
       case lookupRoleRegister r rrs
         of Just var -> return $ fromMaybe Map.empty . cast <$> readTVarIO var
            _        -> []

searchAgentsOfRole' sys r = do regs <- readTVarIO $ _agentsRegisters sys
                               searchAgentsOfRoles regs [r]

searchAgentsOfRoles' sys rs = do regs <- readTVarIO $ _agentsRegisters sys
                                 searchAgentsOfRoles regs rs


-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

instance AgentsManager (SimpleAgentSystem sArgs) where
  listAgents sys = do regs <- readTVarIO $ _agentsRegisters sys
                      maps <- forM regs $
                        \(RoleRegister _ var) ->
                          Map.map someAgentRef <$> readTVarIO var
                      return $ concatMap Map.elems maps
  findAgent sys aId = findAgent' aId =<< readTVarIO (_agentsRegisters sys)


findAgent' _ []                          = return Nothing
findAgent' k (RoleRegister _ var : regs) =
  do agMap <- readTVarIO var
     maybe (findAgent' k regs)
           (return . Just)
           (someAgentRef <$> Map.lookup k agMap)

-----------------------------------------------------------------------------

instance AgentSystem (SimpleAgentSystem sArgs) where
  listAgentsByRole = atomically
                   . (mkAgentsOfRole [] <=< readTVar . _agentsRegisters)
      where
        mkAgentsOfRole acc [] = return . Map.fromList $ reverse acc
        mkAgentsOfRole acc (RoleRegister r var : regs) =
          do rMap <- readTVar var
             let new = AgentsOfRole r $ Map.elems rMap
             mkAgentsOfRole ((SomeRole r, new):acc) regs

  -- listAgentsOfRoles s rs =
  --   maybe [] Map.elems <$> lookupAgentsOfRole' s r

  findAgentOfRole sys r aId = do
    reg <- searchAgentsOfRole' sys r
    return $ Map.lookup aId reg


instance SystemArgsProvider (SimpleAgentSystem sArgs) sArgs where
  agentSysArgs = _agentSysArgs

instance SystemAgentsCreation (SimpleAgentSystem sArgs) sArgs where

  newAgentOfRole s d argsIO = do
    ref <- createAgentRef $ CreateAgentOfRole d (agentSysArgs s) argsIO
    (AgentRoleDescriptor r _) <- return d
    let newRegister =
          do newMap <- newTVar $ Map.singleton (agentId ref) ref
             _agentsRegisters s `modifyTVar` (RoleRegister r newMap :)

    atomically $ do regs <- readTVar $ _agentsRegisters s
                    maybe newRegister
                          (`modifyTVar` Map.insert (agentId ref) ref)
                          (lookupRoleRegister r regs)
    return ref


-----------------------------------------------------------------------------
