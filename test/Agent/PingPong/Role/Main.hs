
module Main where

import Agent.PingPong
import qualified Agent.PingPong.Role.Send       as Send
import qualified Agent.PingPong.Role.Ask        as Ask
import qualified Agent.PingPong.Role.SendResult as SendRes


main = do pingCount <- getPingCount
          putStrLn "== Running Ping-Pong (Send) =="
          Send.runPingPong pingCount
          putStrLn "== Running Ping-Pong (Ask) =="
          Ask.runPingPong pingCount
          putStrLn "== Running Ping-Pong (Send with result) =="
          SendRes.runPingPong pingCount
