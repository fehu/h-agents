-----------------------------------------------------------------------------
--
-- Module      :  Agent.Test.RunPingPongSys
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

import Agent.Test.PingPongSystemSend

import System.Environment



defaultMaxCount = 10

main = do args <- getArgs
          let maxCount = case args of [s] -> read s
                                      _   -> defaultMaxCount

          putStrLn "Testing Ping-Pong Agent System (send)\n"
          testPingPongSys maxCount


