-----------------------------------------------------------------------------
--
-- Module      :  Agent.Test.PingPongSystemSend
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  kdn.kovalev@gmail.com
-- Stability   :
-- Portability :
--
-- |
--

module Agent.Test.PingPongSystemSend where


import Agent.AgentSystem.Default

import Agent.Test.PingPong (pingDescriptor, pongDescriptor)
import Agent.Test.PingPongAgentsSend (pingBehaviour, pongBehaviour)

import Data.IORef
import Data.Maybe (fromJust)

-----------------------------------------------------------------------------

data PingRole = PingRole deriving Show
data PongRole = PongRole deriving Show

data PingAgentState = PingAgentState { pingCounterpart  :: IO AgentRef
                                     , count            :: IORef Int
                                     , firstTime        :: IORef Bool
                                     , pingStatus       :: AgentStatus' ()
                                     }



data PongAgentState = PongAgentState { pongCounterpart :: IO AgentRef
                                     , pongStatus      :: AgentStatus' () }

wTime = Just 100



instance RoleIx PingRole where roleIx _ = 0
instance RoleIx PongRole where roleIx _ = 1

instance EmptyResult () where emptyResult = ()

-----------------------------------------------------------------------------

createPingPongSys pingBehaviour pongBehaviour maxCount = do
    pingRef <- newIORef undefined
    pongRef <- newIORef undefined

    let pingD = pingDescriptor pingBehaviour (readIORef pongRef) maxCount
        pongD = pongDescriptor pongBehaviour (readIORef pingRef) maxCount

    defaultAgentSystem [
          someRoleAgentsDescriptor PingRole wTime [
                CreateAgent pingD (pingStatus . fromJust . extractAgentStates)
                ],
          someRoleAgentsDescriptor PongRole wTime [
                CreateAgent pongD (pongStatus . fromJust . extractAgentStates)
                ]
        ]


testPingPongSys maxCount = do
    sys <- createPingPongSys pingBehaviour pongBehaviour maxCount

    startNegotiation sys

    putStrLn "== Done =="





