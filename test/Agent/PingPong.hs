module Agent.PingPong where

import Data.Typeable

import System.Environment

-----------------------------------------------------------------------------

data Ping = Ping deriving (Typeable, Show)
data Pong = Pong deriving (Typeable, Show)

-----------------------------------------------------------------------------

defaultPingCount = 10
getPingCount = do args <- getArgs
                  return $ case args of [s] -> read s
                                        _   -> defaultPingCount

-----------------------------------------------------------------------------
