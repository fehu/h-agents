-----------------------------------------------------------------------------
--
-- Module      :  AgentRole
-- License     :  MIT
--
-- Maintainer  :  kdn.kovalev@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable (requires STM)
--
-----------------------------------------------------------------------------

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module AgentRole (

  AgentRole(..)
-- , SomeRole(..)

, AgentRoleDescriptor(..)
, CreateAgentOfRole(..)

, module Agent

) where

import Agent

--import Data.Function (on)

-----------------------------------------------------------------------------

class AgentRole r
  where
    roleName :: r -> String

    type RoleState  r :: *
    type RoleResult r :: *
    type RoleArgs   r :: *

-----------------------------------------------------------------------------

--data SomeRole = forall r . AgentRole r => SomeRole r
--roleName' (SomeRole r) = roleName r
--
--instance Show SomeRole where show     = roleName'
--instance Eq   SomeRole where (==)     = (==)    `on` roleName'
--instance Ord  SomeRole where compare  = compare `on` roleName'

-----------------------------------------------------------------------------

data AgentRoleDescriptor r = AgentRoleDescriptor
    r
    (RoleArgs r -> IO (GenericAgentDescriptor (RoleState r) (RoleResult r)))

data CreateAgentOfRole r = CreateAgentOfRole (AgentRoleDescriptor r)
                                             (IO (RoleArgs r))

-----------------------------------------------------------------------------

instance ( RoleState r ~ s, RoleResult r ~ res ) =>
  CreateAgent (CreateAgentOfRole r) res (GenericAgent s res)
    where
      createAgent (CreateAgentOfRole (AgentRoleDescriptor _ create) args) =
        createAgent =<< create =<< args


instance ( RoleResult r ~ res ) =>
  CreateAgentRef (CreateAgentOfRole r) res where
    type CreateAgentType (CreateAgentOfRole r) = GenericAgent (RoleState r)
                                                              (RoleResult r)

-----------------------------------------------------------------------------
