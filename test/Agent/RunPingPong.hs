
module Main where

import qualified Agent.PingPongSend as Send
import qualified Agent.PingPongAsk  as Ask

import System.Environment

defaultPingCount = 10
getPingCount = do args <- getArgs
                  return $ case args of [s] -> read s
                                        _   -> defaultPingCount

main = do pingCount <- getPingCount
          putStrLn "== Running Ping-Pong (Send) =="
          Send.runPingPong pingCount
          putStrLn "== Running Ping-Pong (Ask) =="
          Ask.runPingPong pingCount
