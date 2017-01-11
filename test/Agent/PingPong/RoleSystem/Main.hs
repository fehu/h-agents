
module Main where

import Agent.PingPong
import qualified Agent.PingPong.RoleSystem.SendResult as SendRes


main = do pingCount <- getPingCount
          putStrLn "== Running Ping-Pong (Send with result) =="
          SendRes.runPingPong pingCount
