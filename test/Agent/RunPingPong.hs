
module Main where

import Agent.PingPong
import qualified Agent.PingPongSend       as Send
import qualified Agent.PingPongAsk        as Ask
import qualified Agent.PingPongSendResult as SendRes

import System.Environment

main = do pingCount <- getPingCount
          putStrLn "== Running Ping-Pong (Send) =="
          Send.runPingPong pingCount
          putStrLn "== Running Ping-Pong (Ask) =="
          Ask.runPingPong pingCount
          putStrLn "== Running Ping-Pong (Send with result) =="
          SendRes.runPingPong pingCount
