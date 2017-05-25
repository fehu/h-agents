-----------------------------------------------------------------------------
--
-- Module      :  AgentSystem.Simple.Implementation
-- License     :  MIT
--
-- Maintainer  :  kdn.kovalev@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable (requires STM)
--
-----------------------------------------------------------------------------

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}

module AgentSystem.Simple.Implementation(

  RoleRegister(..)

, simpleRoleRegistersVar, RoleRegistersVar

, simpleAgentsManager_listAgents
, simpleAgentsManager_findAgent

, simpleAgentSystem_listAgentsByRole
, simpleAgentSystem_listAgentsOfRoles
, simpleAgentSystem_findAgentOfRole
, simpleAgentSystem_newAgentOfRole

) where

import AgentSystem

import Data.Maybe (fromMaybe)
import Data.Typeable (Typeable, cast)
import qualified Data.List as List

import Control.Monad ( (<=<), forM )

import Data.Map (Map)
import qualified Data.Map as Map

import Control.Concurrent.STM


-----------------------------------------------------------------------------
{-# ANN module "HLint: ignore Use camelCase" #-}
-----------------------------------------------------------------------------

data RoleRegister = forall r . AgentRole' r =>
    RoleRegister r (TVar (AgentsRoleMap r))

type AgentsRoleMap r = Map AgentId (AgentRef (RoleResult r))

-----------------------------------------------------------------------------

type RoleRegistersVar = TVar [RoleRegister]

simpleRoleRegistersVar :: IO RoleRegistersVar
simpleRoleRegistersVar = newTVarIO []

-----------------------------------------------------------------------------

simpleAgentsManager_listAgents :: (sys -> RoleRegistersVar) -> sys
                               -> IO [SomeAgentRef]
simpleAgentsManager_listAgents registers sys = do
  regs <- readTVarIO $ registers sys
  maps <- forM regs $
    \(RoleRegister _ var) ->
      Map.map someAgentRef <$> readTVarIO var
  return $ concatMap Map.elems maps

simpleAgentsManager_findAgent :: (sys -> RoleRegistersVar) -> sys
                              -> AgentId -> IO (Maybe SomeAgentRef)
simpleAgentsManager_findAgent registers sys aId =
  findAgent' aId =<< readTVarIO (registers sys)

-----------------------------------------------------------------------------

findAgent' _ []                          = return Nothing
findAgent' k (RoleRegister _ var : regs) =
  do agMap <- readTVarIO var
     maybe (findAgent' k regs)
           (return . Just)
           (someAgentRef <$> Map.lookup k agMap)

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

simpleAgentSystem_listAgentsByRole :: (sys -> RoleRegistersVar) -> sys
                                   -> IO AgentsOfRoles
simpleAgentSystem_listAgentsByRole =
  ((atomically . (mkAgentsOfRole [] <=< readTVar)) .)
      where
        mkAgentsOfRole acc [] = return . Map.fromList $ reverse acc
        mkAgentsOfRole acc (RoleRegister r var : regs) =
          do rMap <- readTVar var
             let new = AgentsOfRole r $ Map.elems rMap
             mkAgentsOfRole ((SomeRole r, new):acc) regs


simpleAgentSystem_listAgentsOfRoles :: (Typeable (RoleResult r), AgentRole r) =>
                                       (sys -> RoleRegistersVar) -> sys
                                    -> [r] -> IO [AgentRef (RoleResult r)]
simpleAgentSystem_listAgentsOfRoles registers sys rs =
  Map.elems <$> searchAgentsOfRoles' (registers sys) rs

simpleAgentSystem_findAgentOfRole :: (Typeable (RoleResult r), AgentRole r) =>
                                     (sys -> RoleRegistersVar) -> sys
                                  -> r -> AgentId
                                  -> IO (Maybe (AgentRef (RoleResult r)))
simpleAgentSystem_findAgentOfRole registers sys r aId =
  Map.lookup aId <$> searchAgentsOfRole' (registers sys) r


simpleAgentSystem_newAgentOfRole :: ( AgentControl ag, ReactiveAgent ag
                                    , AgentResult ag (RoleResult r)
                                    , AgentSystemArgsProvider sys (RoleSysArgs r)
                                    , Typeable (RoleResult r), Show (RoleResult r)
                                    , AgentRole r
                                    ) =>
                                    (sys -> RoleRegistersVar) -> sys
                                 -> AgentRoleDescriptor r ag
                                 -> IO (RoleArgs r)
                                 -> IO (AgentRef (RoleResult r))
simpleAgentSystem_newAgentOfRole registers sys d argsIO = do
  ref <- createAgentRef $ CreateAgentOfRole d (agentSysArgs sys) argsIO
  (AgentRoleDescriptor r _) <- return d
  let newRegister =
        do newMap <- newTVar $ Map.singleton (agentId ref) ref
           registers sys `modifyTVar` (RoleRegister r newMap :)

  atomically $ do regs <- readTVar $ registers sys
                  maybe newRegister
                        (`modifyTVar` Map.insert (agentId ref) ref)
                        (lookupRoleRegister r regs)
  return ref


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

searchAgentsOfRole' registersVar r = do regs <- readTVarIO registersVar
                                        searchAgentsOfRoles regs [r]

searchAgentsOfRoles' registersVar rs = do regs <- readTVarIO registersVar
                                          searchAgentsOfRoles regs rs

-----------------------------------------------------------------------------
