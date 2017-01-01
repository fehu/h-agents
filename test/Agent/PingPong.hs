{-# LANGUAGE FlexibleContexts #-}

module Agent.PingPong where

import Agent

import Data.IORef
import Data.Typeable

import Control.Monad (when)
import Control.Concurrent (yield)

--------------------------------------------------------------------------------
_debug = True
--------------------------------------------------------------------------------

data Ping = Ping deriving (Typeable, Show)
data Pong = Pong deriving (Typeable, Show)

--------------------------------------------------------------------------------

-- | First sends 'Ping' message to _pong_ agent.
-- Then sends 'Ping' to _pong_ only in response to 'Pong' message.
-- Total number of 'Ping' messages sent is _nPings_.
pingDescriptor nPings pongRef =
  GenericAgentDescriptor{
    agName  = "Ping"
  , agDebug = _debug
  , initialState = do nPingsRef <- newIORef nPings
                      firstTime <- newIORef True
                      return (nPingsRef, pongRef, firstTime)
  , messageHandling = MessageHandling{
        msgHandle   = selectMessageHandler [
                        mbHandle $ \c Pong -> sendPing c
                      ]
      , msgRespond  = selectResponse []
      }
  , action = \c -> do let (_, _, firstTime') = agentState c
                      firstTime <- readIORef firstTime'
                      when firstTime $ do sendPing c
                                          firstTime' `writeIORef` False
  }

sendPing c = do let (i', pong', _) = agentState c
                i     <- readIORef i'
                pong  <- readIORef pong'
                if i > 0 then do  putStrLn $ "Ping (" ++ show i ++ ")"
                                  i' `writeIORef` (i-1)
                                  pong `send` Ping
                          else do  putStrLn "Terminating Ping"
                                   agentTerminate c

-- | Sends 'Pong' message to _ping_ agent in response to 'Ping'.
pongDescriptor pingRef =
  GenericAgentDescriptor{
    agName  = "Pong"
  , agDebug = _debug
  , initialState = return pingRef
  , messageHandling = MessageHandling{
        msgHandle  = selectMessageHandler [
            mbHandle $ \c Ping -> do ping <- readIORef $ agentState c
                                     putStrLn "Pong"
                                     ping `send` Pong
          ]
      , msgRespond = selectResponse []
      }
  , action = const yield
  }


runPingPong nPings = do pingRef <- newIORef undefined
                        pongRef <- newIORef undefined

                        let pingD = pingDescriptor nPings pongRef
                            pongD = pongDescriptor pingRef

                        ping <- createAgentRef pingD
                        pong <- createAgentRef pongD

                        pingRef `writeIORef` ping
                        pongRef `writeIORef` pong

                        putStrLn "Starting PING"
                        agentStart ping
                        putStrLn "Starting PONG"
                        agentStart pong

                        agentWaitTermination ping
