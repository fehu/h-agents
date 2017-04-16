-----------------------------------------------------------------------------
--
-- Module      :  Agent.Behavior
-- License     :  MIT
--
-- Maintainer  :  kdn.kovalev@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable (requires STM)
--
-----------------------------------------------------------------------------

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}

module Agent.Behavior(

  AgentInnerInterface(..)

-- * Message Handling

, MessageHandling(..), handlesNoMessages, combineMessageHandling

, mbHandle, selectMessageHandler
, mbResp, selectResponse

-- * Action

, AgentAction(..), agentNoAction, runAgentAction

) where

import Agent.Message
import Agent.Interface

import Data.Typeable (cast)

import Control.Exception (Exception)
import Control.Applicative ( (<|>) )
import Control.Concurrent (yield)

-----------------------------------------------------------------------------

class (Agent a res) =>
  AgentInnerInterface a s res | a -> s, a -> res
    where
      agentRef      :: a -> AgentRef res
      agentState    :: a -> s
      agentResultSuccess :: a -> res -> IO ()
      agentResultFailure :: (Exception ex) => a -> ex -> IO ()

-----------------------------------------------------------------------------

data MessageHandling s res = MessageHandling{
    msgHandle   :: forall c msg . (AgentInnerInterface c s res, Message msg) =>
                    c -> msg -> Maybe (IO ())
  , msgRespond  :: forall c msg resp . ( AgentInnerInterface c s res
                                       , MessageResponse msg resp) =>
                    c -> msg -> Maybe (IO resp)
  }

handlesNoMessages = MessageHandling (selectMessageHandler [])
                                    (selectResponse [])


combineMessageHandling :: MessageHandling s res -> MessageHandling s res
                       -> MessageHandling s res
combineMessageHandling h1 h2 = MessageHandling
  (\i msg -> msgHandle  h1 i msg <|> msgHandle  h2 i msg)
  (\i msg -> msgRespond h1 i msg <|> msgRespond h2 i msg)

------------------------------------------------------------------------------

mbHandle  :: (AgentInnerInterface c s res, Message msg0, Message msg)
          => (c -> msg  ->        IO ())
          ->  c -> msg0 -> Maybe (IO ())
mbHandle f c msg = f c <$> cast msg

selectMessageHandler  :: (AgentInnerInterface c s res, Message msg)
                      => [c -> msg -> Maybe (IO ())]
                      ->  c -> msg -> Maybe (IO ())
selectMessageHandler rfs c msg  = foldr (<|>) Nothing
                                $ (\f -> f c msg) <$> rfs


--------------------------------------------------------------------------------

mbResp  :: ( AgentInnerInterface c s res
           , MessageResponse msg0 resp0, MessageResponse msg resp )
        => (c -> msg  ->        IO resp)
        ->  c -> msg0 -> Maybe (IO resp0)
mbResp f c msg = do msg' <- cast msg
                    cast $ f c msg'

selectResponse  :: (AgentInnerInterface c s res, MessageResponse msg resp)
                => [c -> msg -> Maybe (IO resp)]
                ->  c -> msg -> Maybe (IO resp)
selectResponse rfs c msg  = foldr (<|>) Nothing
                        $ (\f -> f c msg) <$> rfs

--------------------------------------------------------------------------------

newtype AgentAction s res = AgentAction {
    agentAct :: forall i . AgentInnerInterface i s res => i -> IO ()
  }

agentNoAction :: AgentAction s res
agentNoAction = AgentAction $ const yield

runAgentAction :: AgentInnerInterface i s res => AgentAction s res -> i -> IO()
runAgentAction (AgentAction act) = act

--------------------------------------------------------------------------------
