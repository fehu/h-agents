-----------------------------------------------------------------------------
--
-- Module      :  GenericAgent.Test.PingPong
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  kdn.kovalev@gmail.com
-- Stability   :
-- Portability :
--
-- |
--

{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}

module Agent.Test.PingPong where

import Agent
import Agent.Extra

import Data.IORef
import Data.Typeable

import Control.Monad (when)

-----------------------------------------------------------------------------

data Ping = Ping deriving (Show, Typeable)
data Pong = Pong deriving (Show, Typeable)

-----------------------------------------------------------------------------


data PingAgentState = PingAgentState { pingCounterpart  :: IO AgentRef
                                     , count            :: IORef Int
                                     , firstTime        :: IORef Bool
                                     }



data PongAgentState = PongAgentState { pongCounterpart :: IO AgentRef }


-----------------------------------------------------------------------------

pingDescriptor pingBehaviour counterpart maxCount = AgentDescriptor{
    agentDefaultBehaviour = pingBehaviour maxCount
  , newAgentStates = do count <- newIORef 0
                        first <- newIORef True
                        return $ PingAgentState counterpart count first
  , nextAgentId    = return $ AgentId "Ping"
  , noResult       = ()
  , debugAgent = True
  }


pongDescriptor pongBehaviour counterpart _ = AgentDescriptor{
    agentDefaultBehaviour = pongBehaviour
  , newAgentStates        = return $ PongAgentState counterpart
  , nextAgentId           = return $ AgentId "Pong"
  , noResult              = ()
  , debugAgent            = True
}
-----------------------------------------------------------------------------

createPingPong' pingBehaviour pongBehaviour maxCount =
    do pingRef <- newIORef undefined
       pongRef <- newIORef undefined

       let pingD = pingDescriptor pingBehaviour (readIORef pongRef) maxCount
           pongD = pongDescriptor pongBehaviour (readIORef pingRef) maxCount

--       m  <- newAgentsManager :: IO StatelessAgentsManager

       (ping :: ExecutableAgent, piFRref) <- createAgent pingD
       (pong :: ExecutableAgent, poFRref) <- createAgent pongD

       pingRef `writeIORef` fromFullRef piFRref
       pongRef `writeIORef` fromFullRef poFRref

       return (piFRref, poFRref)


testPingPong' pingBehaviour pongBehaviour maxCount =
    do (ping, pong) <- createPingPong' pingBehaviour pongBehaviour maxCount
       ping `sendPriority` StartMessage
       pong `sendPriority` StartMessage

       putStrLn "Stopping"
       ping `send` StopMessage
       pong `send` StopMessage

       putStrLn "Waiting for Ping"
       waitAgent ping

       return "Done"

-----------------------------------------------------------------------------

whenM :: (Monad m) => m Bool -> m () -> m ()
whenM mb a = (`when` a) =<< mb
