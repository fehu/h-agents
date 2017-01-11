{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Agent.PingPong.SimpleSystem.SendResult where

import AgentSystem
import Agent.PingPong
import Agent.PingPong.Simple.SendResult (Finished(..))
import qualified Agent.PingPong.SimpleSystem.Send as Send

-- import Control.Monad (when)
import Control.Concurrent (yield)

import Data.IORef

--------------------------------------------------------------------------------

-- | First sends 'Ping' message to _pong_ agent.
-- Then sends 'Ping' to _pong_ only in response to 'Pong' message.
-- After sending _nPings_ 'Ping' messages, sends 'Finished' message
-- and terminates.
pingDescriptor sys nPings =
    let original = Send.pingDescriptor sys nPings
    in original {
        messageHandling = MessageHandling{
            msgHandle   = selectMessageHandler [
                            mbHandle $ \c Pong -> sendPing c
                          ]
          , msgRespond  = selectResponse []
          }
    }

sendPing c = do let (i', sys, _) = agentState c
                i <- readIORef i'
                Just pong <- sys `findAgent` "Pong"
                if i > 0 then do  putStrLn $ "Ping (" ++ show i ++ ")"
                                  pong `send` Ping
                                  i' `writeIORef` (i-1)
                          else do  putStrLn "Ping finished"
                                   pong `send` Finished
                                   putStrLn "Terminating Ping"
                                   agentTerminate c


-- | Sends 'Pong' message to _ping_ agent in response to 'Ping'.
-- Terminates after receiving 'Finished' message.
pongDescriptor sys =
  let original = Send.pongDescriptor sys
  in original {
    messageHandling = MessageHandling{
        msgHandle  = selectMessageHandler [
            mbHandle $ \c Ping     -> do Just ping <- agentState c `findAgent` "Ping"
                                         putStrLn "Pong"
                                         ping `send` Pong
          , mbHandle $ \c Finished -> do putStrLn $ "Pong received 'Finished' message"
                                                 ++ "\nTerminating Pong"
                                         agentTerminate c
          ]
      , msgRespond = selectResponse []
      }
  }

--------------------------------------------------------------------------------

runPingPong nPings = do putStrLn "<< Create Simple System >> "
                        sys  <- newSimpleAgentSystem
                        ping <- sys `newAgent` pingDescriptor sys nPings
                        pong <- sys `newAgent` pongDescriptor sys

                        putStrLn "Starting system"
                        startAllAgents sys

                        putStrLn "Waiting system termination"
                        AgentSystem.waitAllAgents sys
                        putStrLn "Finished"
