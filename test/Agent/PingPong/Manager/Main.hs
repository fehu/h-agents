
module Main where

import Agent.PingPong
import qualified Agent.PingPong.Manager.Send       as Send
import qualified Agent.PingPong.Manager.Ask        as Ask


main = do pingCount <- getPingCount
          putStrLn "== Running Ping-Pong Manager (Send) =="
          Send.runPingPong pingCount
          putStrLn "== Running Ping-Pong Manager (Ask) =="
          Ask.runPingPong pingCount
