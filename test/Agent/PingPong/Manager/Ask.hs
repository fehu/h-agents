{-# LANGUAGE TypeFamilies #-}

module Agent.PingPong.Manager.Ask where


import Agent
import AgentSystem.Manager
import Agent.PingPong

import qualified Agent.PingPong.Simple.Ask as Ask

import Data.IORef

import Control.Concurrent (yield)

--------------------------------------------------------------------------------

type instance ExpectedResponse Ping = Pong


runPingPong nPings = do
  pongRef <- newIORef undefined

  putStrLn "<< Create Simple Manager >> "
  m  <- newSimpleAgentsManager
  ping <- m `newAgent` Ask.pingDescriptor nPings pongRef
  pong <- m `newAgent` Ask.pongDescriptor

  pongRef `writeIORef` pong

  putStrLn "Starting agents"
  startAllAgents m

  putStrLn "Waiting PING termination"
  agentWaitTermination ping

  putStrLn "Finished"
