-----------------------------------------------------------------------------
--
-- Module      :  AgentSystem.Role
-- License     :  MIT
--
-- Maintainer  :  kdn.kovalev@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ConstraintKinds #-}

module AgentSystem.Role(

  AgentRole(..), RoleName(..), RoleT(..)
, isSameRole
, SomeRole(..)

, AgentOfRole, AgentRefOfRole

, AgentRoleDescriptor(..), unsafeModifyAgentRoleDescriptor

, CreateAgentOfRole(..)

, module Agent

) where

import Agent

import Data.Maybe (fromJust)
import Data.Function (on)
import Data.Typeable (Typeable, cast)

import Control.Monad ( (<=<) )

-----------------------------------------------------------------------------

class RoleName r where roleName :: r -> String

class (RoleName r) => AgentRole r
  where
    type RoleState  r :: *
    type RoleResult r :: *
    type RoleArgs   r :: *

-----------------------------------------------------------------------------

newtype RoleT r a = RoleT r
instance (RoleName r) => RoleName (RoleT r a)
  where roleName (RoleT r) = roleName r

-----------------------------------------------------------------------------


data SomeRole = forall r . AgentRole r => SomeRole r
roleName' (SomeRole r) = roleName r

isSameRole r1 r2 = roleName r1 == roleName r2

instance Show SomeRole where show     = roleName'
instance Eq   SomeRole where (==)     = (==) `on` roleName'
instance Ord  SomeRole where compare  = compare `on` roleName'

-----------------------------------------------------------------------------

type AgentOfRole a r = Agent a (RoleResult r)
type AgentRefOfRole r = AgentRef (RoleResult r)

-----------------------------------------------------------------------------

-- data AgentRoleDescriptor r ag = forall from . ( CreateAgent from (RoleResult r) ag ) =>
--   AgentRoleDescriptor r (RoleArgs r -> IO from)

data AgentRoleDescriptor r ag = forall from . ( CreateAgent from (RoleResult r) ag
                                              , Typeable from) =>
  AgentRoleDescriptor r (RoleArgs r -> IO from)

data CreateAgentOfRole r ag = CreateAgentOfRole (AgentRoleDescriptor r ag)
                                                (IO (RoleArgs r))

unsafeModifyAgentRoleDescriptor :: (Typeable d, CreateAgent d (RoleResult r) ag) =>
                                   AgentRoleDescriptor r ag -> (d -> d)
                                -> AgentRoleDescriptor r ag
unsafeModifyAgentRoleDescriptor (AgentRoleDescriptor r create) f =
  AgentRoleDescriptor r (fmap (f . fromJust . cast) . create)

-----------------------------------------------------------------------------

instance ( RoleResult r ~ res ) =>
  CreateAgent (CreateAgentOfRole r ag) res ag
    where
      createAgent (CreateAgentOfRole (AgentRoleDescriptor _ create) args) =
        createAgent =<< create =<< args

instance ( Agent ag res, RoleResult r ~ res ) =>
  CreateAgentRef (CreateAgentOfRole r ag) res where
    type CreateAgentType (CreateAgentOfRole r ag) = ag

-----------------------------------------------------------------------------
