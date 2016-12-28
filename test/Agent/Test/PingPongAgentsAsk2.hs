-----------------------------------------------------------------------------
--
-- Module      :  GenericAgent.Test.PingPongAgentsAskT
-- License     :  MIT
--
-- |
--

{-# LANGUAGE TypeFamilies, FlexibleContexts, ScopedTypeVariables #-}

module Agent.Test.PingPongAgentsAsk2 where

import Agent
import Agent.Test.PingPong hiding (Ping, Pong)

import Data.Typeable (Typeable)

-----------------------------------------------------------------------------

newtype Ping = Ping Int deriving (Typeable, Show)
newtype Pong = Pong Int deriving (Typeable, Show)

type instance ExpectedResponse Ping = Pong

-----------------------------------------------------------------------------

askPing maxCount c i state = do counterpart <- pingCounterpart state
                                putStrLn $ "c = " ++ show c
                                if c < maxCount
                                 then do putStrLn "Ping!"
                                         Pong c' <- counterpart `ask` Ping c
                                         askPing maxCount c' i state
                                 else do putStrLn "Finished"
                                         agentTerminate i

-- | TODO
pingBehaviour :: Int -> AgentBehavior PingAgentState
pingBehaviour maxCount = AgentBehavior{
    handleMessages = AgentHandleMessages {
            handleMessage       = \_ _ -> undefined
          , respondMessage      = \_ _ -> undefined
          }
  , agentAct = AgentActRepeat (askPing maxCount 0) Nothing
  }


-----------------------------------------------------------------------------

-- | TODO
pongBehaviour = AgentBehavior{
  handleMessages = AgentHandleMessages{
          handleMessage  = \_ _ -> undefined
        , respondMessage = \_ _ -> selectResponse [
              mbResp $ \(Ping c) -> return $ Pong (c + 1)
            ]
        }
  , agentAct = AgentNoAct
  }


-----------------------------------------------------------------------------

createPingPong = createPingPong' pingBehaviour pongBehaviour

testPingPong = testPingPong' pingBehaviour pongBehaviour

