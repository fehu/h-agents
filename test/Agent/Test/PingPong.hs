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
import Agent.Manager

import Data.IORef
import Data.Typeable

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
  , nextAgentId    = const . return $ AgentId "Ping"
  , noResult       = ()
  , debugAgent = True
  }


pongDescriptor pongBehaviour counterpart _ = AgentDescriptor{
    agentDefaultBehaviour = pongBehaviour
  , newAgentStates        = return $ PongAgentState counterpart
  , nextAgentId           = const . return $ AgentId "Pong"
  , noResult              = ()
  , debugAgent            = True
}
-----------------------------------------------------------------------------

createPingPong' pingBehaviour pongBehaviour maxCount =
    do pingRef <- newIORef undefined
       pongRef <- newIORef undefined

       let pingD = pingDescriptor pingBehaviour (readIORef pongRef) maxCount
           pongD = pongDescriptor pongBehaviour (readIORef pingRef) maxCount

       m  <- newAgentsManager :: IO StatelessAgentsManager

       (ping :: GenericAgent, (piFRref, _)) <- createWithManager m pingD (const $ return ())
       (pong :: GenericAgent, (poFRref, _)) <- createWithManager m pongD (const $ return ())

       pingRef `writeIORef` fromFullRef piFRref
       pongRef `writeIORef` fromFullRef poFRref

       return (m, (piFRref, poFRref))


testPingPong' pingBehaviour pongBehaviour maxCount =
    do (m, (ping, pong)) <- createPingPong' pingBehaviour pongBehaviour maxCount
       ping `sendPriority` StartMessage
       pong `sendPriority` StartMessage

       putStrLn "Waiting for Ping"
       waitAgent ping

       putStrLn "Stopping"
       m `orderEachAgent` StopMessage

       return "Done"

