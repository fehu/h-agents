{-# LANGUAGE TypeFamilies #-}

module Agent.PingPong.SimpleSystem.Ask where


import AgentSystem
import Agent.PingPong

import qualified Agent.PingPong.Simple.Ask as Ask

import Data.IORef

import Control.Concurrent (yield)

--------------------------------------------------------------------------------

type instance ExpectedResponse Ping = Pong


runPingPong nPings = do
  pongRef <- newIORef undefined

  putStrLn "<< Create Simple System >> "
  sys  <- newAgentSystem :: IO SimpleAgentSystem
  ping <- sys `newAgent` Ask.pingDescriptor nPings pongRef
  pong <- sys `newAgent` Ask.pongDescriptor

  pongRef `writeIORef` pong

  putStrLn "Starting system"
  startAllAgents sys

  putStrLn "Waiting PING termination"
  agentWaitTermination ping

  putStrLn "Finished"
