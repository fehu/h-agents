
module Main where

import Agent.PingPong
import qualified Agent.PingPong.Simple.Send       as Send
import qualified Agent.PingPong.Simple.Ask        as Ask
import qualified Agent.PingPong.Simple.SendResult as SendRes

import System.Environment

main = do pingCount <- getPingCount
          putStrLn "== Running Ping-Pong (Send) =="
          Send.runPingPong pingCount
          putStrLn "== Running Ping-Pong (Ask) =="
          Ask.runPingPong pingCount
          putStrLn "== Running Ping-Pong (Send with result) =="
          SendRes.runPingPong pingCount
