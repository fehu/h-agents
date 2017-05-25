-----------------------------------------------------------------------------
--
-- Module      :  AgentSystem.Simple
-- License     :  MIT
--
-- Maintainer  :  kdn.kovalev@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable (requires STM)
--
-----------------------------------------------------------------------------

{-# LANGUAGE TemplateHaskell #-}
-- {-# OPTIONS_GHC -ddump-splices #-}

module AgentSystem.Simple(

  SimpleAgentSystem, newSimpleAgentSystem

) where

import AgentSystem as Export
import AgentSystem.Simple.Template

import Control.Concurrent.STM

-----------------------------------------------------------------------------

newtype SimpleAgentSystem = SimpleAgentSystem{
  _agentsRegisters :: TVar [RoleRegister]
  }

newSimpleAgentSystem = SimpleAgentSystem <$> simpleRoleRegistersVar

-----------------------------------------------------------------------------

genAgentSystemInstances "SimpleAgentSystem" "_agentsRegisters"

-----------------------------------------------------------------------------
