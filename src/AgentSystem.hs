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
{-# LANGUAGE FlexibleInstances #-}


module AgentSystem(

  AgentSystem(..), AgentSystemArgsProvider(..)
, AgentRole', AgentsOfRole(..), AgentsOfRoles
, AgentResultsOfRoles, AgentMaybeResultsOfRoles

, module Export

) where

import AgentSystem.Manager as Export
import AgentSystem.Role    as Export

import Data.Typeable (Typeable)

import Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad ( (<=<) )

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

  newAgentOfRole :: forall r ag . ( AgentRole' r, AgentOfRole ag r
                                  , AgentSystemArgsProvider sys (RoleSysArgs r)) =>
                    sys
                 -> AgentRoleDescriptor r ag
                 -> IO (RoleArgs r)
                 -> IO (AgentRef (RoleResult r))

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

class AgentSystemArgsProvider sys sargs where
  agentSysArgs :: sys -> IO sargs

instance AgentSystemArgsProvider sys () where agentSysArgs = const $ return ()

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
