-----------------------------------------------------------------------------
--
-- Module      :  Agent
-- License     :  MIT
--
-- Maintainer  :  kdn.kovalev@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable (requires STM)
--
-----------------------------------------------------------------------------

module Agent(

  Message, ExpectedResponse, MessageResponse

, ResponseInterface(..), Response
, ResponseProvider(..), Respond
, ResponsePromise(..)

, AgentId(..)
, Agent
, ReactiveAgent(..)
, AgentControl(..)
, AgentResult(..)
, AgentExecutionResult, AgentExecutionAborted(..)

, AgentRef, SomeAgentRef, someAgentRef

, CreateAgent(..), CreateAgentRef(..)

) where

import Agent.Message
import Agent.Interface

-----------------------------------------------------------------------------
