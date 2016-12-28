-----------------------------------------------------------------------------
--
-- Module      :  GenericAgent.Test.RunPingPong
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  kdn.kovalev@gmail.com
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Main where

import qualified Agent.Test.PingPongAgentsSend as PPASend
import qualified Agent.Test.PingPongAgentsAsk  as PPAAsk
import qualified Agent.Test.PingPongAgentsAsk2 as PPAAsk2
import qualified Agent.Test.PingPongSystemSend as PPSSend

import System.Environment

import Control.Exception
import Control.Monad

-----------------------------------------------------------------------------


defaultMaxCount = 10

main = do args <- getArgs
          let maxCount = case args of [s] -> read s
                                      _   -> defaultMaxCount

          putStrLn "Testing Ping-Pong (send)\n"
          exec $ PPASend.testPingPong maxCount

          putStrLn "\n\nTesting Ping-Pong (ask)\n"
          exec $ PPAAsk.testPingPong maxCount

          putStrLn "\n\nTesting Ping-Pong (ask 2)\n"
          exec $ PPAAsk2.testPingPong maxCount


exec f = do res <- try f
            putStrLn $  case res of Left (SomeException ex) -> "Test Error: " ++ show ex
                                    Right _                 -> "Test OK"
