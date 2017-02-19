{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Agent.PingPong.Manager.Send where

import Agent.Generic
import AgentSystem.Manager
import Agent.PingPong

import Control.Monad (when)
import Control.Concurrent (yield)

import Data.IORef

--------------------------------------------------------------------------------

-- | First sends 'Ping' message to _pong_ agent.
-- Then sends 'Ping' to _pong_ only in response to 'Pong' message.
-- Total number of 'Ping' messages sent is _nPings_.
pingDescriptor sys nPings =
  GenericAgentDescriptor{
    agName  = "Ping"
  , agDebug = _debug
  , initialState = do nPingsRef <- newIORef nPings
                      firstTime <- newIORef True
                      return (nPingsRef, sys, firstTime)
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
  , emptyResult = EmptyResult :: EmptyResult ()
  }

sendPing c = do let (i', sys, _) = agentState c
                i   <- readIORef i'
                Just pong <- sys `findAgent` "Pong"
                if i > 0 then do  putStrLn $ "Ping (" ++ show i ++ ")"
                                  pong `send` Ping
                                  i' `writeIORef` (i-1)
                          else do  putStrLn "Terminating Ping"
                                   agentTerminate c

-- | Sends 'Pong' message to _ping_ agent in response to 'Ping'.
pongDescriptor sys =
  GenericAgentDescriptor{
    agName  = "Pong"
  , agDebug = _debug
  , initialState = return sys
  , messageHandling = MessageHandling{
        msgHandle  = selectMessageHandler [
            mbHandle $ \c Ping -> do Just ping <- sys `findAgent` "Ping"
                                     putStrLn "Pong"
                                     ping `send` Pong
          ]
      , msgRespond = selectResponse []
      }
  , action = const yield
  , emptyResult = EmptyResult :: EmptyResult ()
  }


--------------------------------------------------------------------------------

runPingPong nPings = do putStrLn "<< Create Agents Manager >> "
                        m  <- newSimpleAgentsManager
                        ping <- m `newAgent` pingDescriptor m nPings
                        pong <- m `newAgent` pongDescriptor m

                        putStrLn "Starting agents"
                        startAllAgents m

                        putStrLn "Waiting PING termination"
                        agentWaitTermination ping

                        putStrLn "Terminating agents"
                        terminateAllAgents m
                        putStrLn "Finished"
