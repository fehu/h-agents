-----------------------------------------------------------------------------
--
-- Module      :  AgentSystem.Generic
-- License     :  MIT
--
-- Maintainer  :  kdn.kovalev@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable (requires STM)
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module AgentSystem.Generic (

  GenericAgentOfRole, GenericRoleDescriptor, genericRoleDescriptor

, GenericAgentOfRoleDescriptor, unsafeModifyGenericRoleDescriptor

, module Export

) where

import AgentSystem   as Export
import Agent.Generic as Export

import Data.Typeable (Typeable)

import Control.Monad ( (<=<) )

import Data.Function (on)

-----------------------------------------------------------------------------

type GenericAgentOfRole r = GenericAgent (RoleState r) (RoleResult r)

type GenericRoleDescriptor r = AgentRoleDescriptor r (GenericAgentOfRole r)

genericRoleDescriptor :: ( CreateAgent from (RoleResult r) (GenericAgentOfRole r)
                         , Typeable from
                         ) =>
                         r -> (RoleSysArgs r -> RoleArgs r -> IO from)
                           -> GenericRoleDescriptor r
genericRoleDescriptor = AgentRoleDescriptor

type GenericAgentOfRoleDescriptor r = GenericAgentDescriptor (RoleState r) (RoleResult r)

-----------------------------------------------------------------------------

unsafeModifyGenericRoleDescriptor :: (Typeable (RoleState r), Typeable (RoleResult r)) =>
     (GenericAgentOfRoleDescriptor r -> GenericAgentOfRoleDescriptor r)
  -> GenericRoleDescriptor r -> GenericRoleDescriptor r
unsafeModifyGenericRoleDescriptor = unsafeModifyAgentRoleDescriptor

-----------------------------------------------------------------------------
