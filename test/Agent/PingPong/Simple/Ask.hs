{-# LANGUAGE TypeFamilies #-}

module Agent.PingPong.Simple.Ask where


import Agent.Generic
import Agent.PingPong

import Data.IORef

import Control.Concurrent (yield)

--------------------------------------------------------------------------------

type instance ExpectedResponse Ping = Pong


-- | Ask 'Ping' _nPings_ times. Handles no messages.
pingDescriptor nPings pongRef =
  GenericAgentDescriptor{
      agName  = "Ping"
    , agDebug = _debug
    , initialState = do nPingsRef <- newIORef nPings
                        return (nPingsRef, pongRef)
    , messageHandling = handlesNoMessages
    , action = AgentAction $
               \c -> do let (i', pong') = agentState c
                        i     <- readIORef i'
                        pong  <- readIORef pong'
                        if i > 0 then do putStrLn $ "Ping (" ++ show i ++ ")"
                                         resp <- pong `ask` Ping
                                         Pong <- waitResponseSuccess resp
                                         putStrLn "Ping: response received"
                                         i' `writeIORef` (i-1)
                                 else do putStrLn "Terminating Ping"
                                         agentTerminate c
    , emptyResult = EmptyResult :: EmptyResult ()
    }

-- | Responds 'Pong' to 'Ping' message.
pongDescriptor =
  GenericAgentDescriptor{
      agName = "Pong"
    , agDebug = _debug
    , initialState = return ()
    , emptyResult  = EmptyResult :: EmptyResult ()
    , messageHandling = MessageHandling{
          msgHandle  = selectMessageHandler []
        , msgRespond = selectResponse [
              mbResp $ \_ Ping -> putStrLn "Pong" >> return Pong
            ]
        }
    , action = agentNoAction
    }


runPingPong nPings = do
  pongRef <- newIORef undefined

  let pingD = pingDescriptor nPings pongRef
      pongD = pongDescriptor

  ping <- createAgentRef pingD
  pong <- createAgentRef pongD

  pongRef `writeIORef` pong

  putStrLn "Starting PING"
  agentStart ping
  putStrLn "Starting PONG"
  agentStart pong

  putStrLn "Waiting PING termination"
  agentWaitTermination ping
