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

  AgentRole(..), RoleName(..) -- , RoleT(..) -- , roleT
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

-- import Data.List (intercalate)
-- import Data.Set (Set)

-- import qualified Data.Set as Set

import Control.Monad ( (<=<) )

-----------------------------------------------------------------------------

class RoleName r where
  roleName :: r -> String
  roleCmp  :: (RoleName r0) => r0 -> r -> Ordering
  roleEq   :: (RoleName r0) => r0 -> r -> Bool

  -- | By default roles are compared by name
  roleCmp x y = roleName x `compare` roleName y
  -- | Based on 'roleCmp'
  roleEq x y = roleCmp x y == EQ

class (RoleName r) => AgentRole r
  where
    type RoleState  r :: *
    type RoleResult r :: *
    type RoleArgs   r :: *

-----------------------------------------------------------------------------

-- newtype RoleT r a = RoleT r
-- instance (RoleName r) => RoleName (RoleT r a) where roleName (RoleT r) = roleName r

-- newtype RoleT r a = RoleT (Set r)
-- instance (RoleName r) => RoleName (RoleT r a)
--   where roleName (RoleT rs) = intercalate ", " $ roleName <$> Set.toList rs
--
-- roleT :: (Ord r) => [r] -> RoleT r a
-- roleT = RoleT . Set.fromList

-----------------------------------------------------------------------------


data SomeRole = forall r . AgentRole r => SomeRole r

instance RoleName SomeRole where roleName (SomeRole r) = roleName r
instance Show     SomeRole where show = roleName
instance Eq       SomeRole where (==)     = roleEq
instance Ord      SomeRole where compare  = roleCmp

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
                                   (d -> d)
                                -> AgentRoleDescriptor r ag
                                -> AgentRoleDescriptor r ag
unsafeModifyAgentRoleDescriptor f (AgentRoleDescriptor r create) =
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
