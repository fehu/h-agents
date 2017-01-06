{-# LANGUAGE FlexibleContexts #-}

module Agent.PingPong.Simple.SendResult where

import Agent
import Agent.PingPong
import qualified Agent.PingPong.Simple.Send as Send

import Data.Typeable (Typeable)
import Data.IORef

import Control.Concurrent (yield)

--------------------------------------------------------------------------------

data Finished = Finished deriving (Typeable, Show)


-- | First sends 'Ping' message to _pong_ agent.
-- Then sends 'Ping' to _pong_ only in response to 'Pong' message.
-- After sending _nPings_ 'Ping' messages, sends 'Finished' message
-- and terminates.
pingDescriptor nPings pongRef =
    let original = Send.pingDescriptor nPings pongRef
    in original {
        messageHandling = MessageHandling{
            msgHandle   = selectMessageHandler [
                            mbHandle $ \c Pong -> sendPing c
                          ]
          , msgRespond  = selectResponse []
          }
    }

sendPing c = do let (i', pong', _) = agentState c
                i     <- readIORef i'
                pong  <- readIORef pong'
                if i > 0 then do  putStrLn $ "Ping (" ++ show i ++ ")"
                                  pong `send` Ping
                                  i' `writeIORef` (i-1)
                          else do  putStrLn "Ping finished"
                                   pong `send` Finished
                                   putStrLn "Terminating Ping"
                                   agentTerminate c


-- | Sends 'Pong' message to _ping_ agent in response to 'Ping'.
-- Counts received 'Ping' messages and reports it as a result
-- after receiving 'Finished' message. Terminates after that.

pongDescriptor pingRef =
  GenericAgentDescriptor{
    agName  = "Pong"
  , agDebug = _debug
  , initialState = do countVar <- newIORef 0
                      return (countVar, pingRef)
  , messageHandling = MessageHandling{
        msgHandle  = selectMessageHandler [
            mbHandle $ \c Ping     -> do let (count', ping') = agentState c
                                         ping <- readIORef ping'
                                         putStrLn "Pong"
                                         count' `modifyIORef` (+1)
                                         ping `send` Pong
          , mbHandle $ \c Finished -> do let (count', _) = agentState c
                                         count <- readIORef count'
                                         putStrLn "Pong received 'Finished' message"
                                         c `agentResultSuccess` count
                                         putStrLn "Terminating Pong"
                                         agentTerminate c
          ]
      , msgRespond = selectResponse []
      }
  , action = const yield
  , emptyResult = EmptyResult :: EmptyResult Int
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

                        putStrLn "Waiting PING termination"
                        agentWaitTermination ping
                        putStrLn "Waiting PONG result"
                        res <- agentWaitResult pong

                        putStrLn $ "PONG result: " ++ show res

                        putStrLn "Waiting PONG termination"
                        agentWaitTermination pong

                        putStrLn "Finished"
