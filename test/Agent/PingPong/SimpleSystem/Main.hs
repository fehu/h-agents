
module Main where

import Agent.PingPong
import qualified Agent.PingPong.SimpleSystem.Send       as Send
import qualified Agent.PingPong.SimpleSystem.Ask        as Ask


main = do pingCount <- getPingCount
          putStrLn "== Running Ping-Pong (Send) =="
          Send.runPingPong pingCount
          putStrLn "== Running Ping-Pong (Ask) =="
          Ask.runPingPong pingCount
