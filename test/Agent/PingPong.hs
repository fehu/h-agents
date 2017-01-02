module Agent.PingPong where

import Data.Typeable

data Ping = Ping deriving (Typeable, Show)
data Pong = Pong deriving (Typeable, Show)
