-----------------------------------------------------------------------------
--
-- Module      :  Agent.Interface
-- License     :  MIT
--
-- Maintainer  :  kdn.kovalev@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable (requires STM)
--
-----------------------------------------------------------------------------

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}

module Agent.Interface(

  Agent, AgentId(..)
, ReactiveAgent(..)
, AgentControl(..)
, AgentResult(..)
, AgentExecutionResult, AgentExecutionAborted(..)

, AgentRef, SomeAgentRef, someAgentRef

, CreateAgent(..), CreateAgentRef(..)

) where

import Agent.Message

import Data.Function (on)
import Data.Typeable (Typeable)

import Control.Monad (when)
import Control.Exception (Exception, SomeException)


------------------------------------------------------------------------------

newtype AgentId = AgentId { rawAgentId :: String } deriving (Show, Eq, Ord)

type Agent a res = (ReactiveAgent a, AgentControl a, AgentResult a res)

------------------------------------------------------------------------------

-- | Can receive and respond messages.
--   Priority mesages are handled before the normal ones.
class ReactiveAgent a where
  send :: (Message msg) => a -> msg -> IO ()
  ask  :: (MessageResponse msg resp) => a -> msg -> IO (Response resp)

  sendPriority :: (Message msg) => a -> msg -> IO ()
  askPriority  :: (MessageResponse msg resp) => a -> msg -> IO (Response resp)


class AgentControl a
  where agentStart            :: a -> IO ()
        agentPause            :: a -> IO ()
        agentTerminate        :: a -> IO ()
        agentWaitTermination  :: a -> IO ()

        agentRunning    :: a -> IO Bool
        agentPaused     :: a -> IO Bool
        agentTerminated :: a -> IO Bool

        agentId       :: a -> AgentId
        agentDebug    :: a -> IO Bool
        agentSetDebug :: a -> Bool -> IO ()
        printDebug    :: a -> String -> IO ()

        printDebug a str = do debug <- agentDebug a
                              when debug . putStrLn $
                                "[DEBUG][" ++ show (agentId a) ++ "] " ++ str


class AgentResult a res | a -> res
  where
    agentResult     :: a -> IO (Maybe (AgentExecutionResult res))
    agentWaitResult :: a -> IO        (AgentExecutionResult res)


------------------------------------------------------------------------------

type AgentExecutionResult res = Either SomeException res

data AgentExecutionAborted = AgentExecutionAborted deriving (Typeable, Show)
instance Exception AgentExecutionAborted

------------------------------------------------------------------------------

-- | Reference for 'ReactiveAgent' with 'AgentControl' and specific 'AgentResult'.
data AgentRef res = forall a . Agent a res => AgentRef a

instance ReactiveAgent (AgentRef res) where
  send (AgentRef a) = send a
  ask  (AgentRef a) = ask a
  sendPriority (AgentRef a) = sendPriority a
  askPriority  (AgentRef a) = askPriority a

instance AgentControl (AgentRef res) where
  agentId              (AgentRef a) = agentId a
  agentDebug           (AgentRef a) = agentDebug a
  agentSetDebug        (AgentRef a) = agentSetDebug a
  agentStart           (AgentRef a) = agentStart a
  agentPause           (AgentRef a) = agentPause a
  agentTerminate       (AgentRef a) = agentTerminate a
  agentWaitTermination (AgentRef a) = agentWaitTermination a

  agentRunning         (AgentRef a) = agentRunning a
  agentPaused          (AgentRef a) = agentPaused a
  agentTerminated      (AgentRef a) = agentTerminated a

instance AgentResult (AgentRef res) res where
  agentResult     (AgentRef a) = agentResult a
  agentWaitResult (AgentRef a) = agentWaitResult a


instance Eq  (AgentRef res) where (==)    = (==)    `on` agentId
instance Ord (AgentRef res) where compare = compare `on` agentId

instance Show (AgentRef res) where show ref = "AgentRef " ++ show (agentId ref)

------------------------------------------------------------------------------

-- | Reference for 'ReactiveAgent' with 'AgentControl' and some 'AgentResult'.
data SomeAgentRef = forall a res . Agent a res => SomeAgentRef a

someAgentRef :: AgentRef res -> SomeAgentRef
someAgentRef (AgentRef a) = SomeAgentRef a

instance ReactiveAgent SomeAgentRef where
  send (SomeAgentRef a) = send a
  ask  (SomeAgentRef a) = ask a
  sendPriority (SomeAgentRef a) = sendPriority a
  askPriority  (SomeAgentRef a) = askPriority a

instance AgentControl SomeAgentRef where
  agentId              (SomeAgentRef a) = agentId a
  agentDebug           (SomeAgentRef a) = agentDebug a
  agentSetDebug        (SomeAgentRef a) = agentSetDebug a
  agentStart           (SomeAgentRef a) = agentStart a
  agentPause           (SomeAgentRef a) = agentPause a
  agentTerminate       (SomeAgentRef a) = agentTerminate a
  agentWaitTermination (SomeAgentRef a) = agentWaitTermination a

  agentRunning         (SomeAgentRef a) = agentRunning a
  agentPaused          (SomeAgentRef a) = agentPaused a
  agentTerminated      (SomeAgentRef a) = agentTerminated a


instance Eq  SomeAgentRef where (==)    = (==)    `on` agentId
instance Ord SomeAgentRef where compare = compare `on` agentId

instance Show SomeAgentRef where
  show (SomeAgentRef ref) = "AgentRef " ++ show (agentId ref)

------------------------------------------------------------------------------

-- | Agent is created from the corresponding descriptor.
class CreateAgent from res a | from -> res
  where
    createAgent :: from -> IO a

class ( CreateAgent from res (CreateAgentType from)
      , ReactiveAgent        (CreateAgentType from)
      , AgentControl         (CreateAgentType from)
      , AgentResult          (CreateAgentType from) res
      ) =>
  CreateAgentRef from res where
    type CreateAgentType from :: *
    createAgentRef :: from -> IO (AgentRef res)
    createAgentRef from = do (ag :: CreateAgentType from) <- createAgent from
                             return $ AgentRef ag

------------------------------------------------------------------------------
