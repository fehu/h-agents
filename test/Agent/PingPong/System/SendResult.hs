{-# LANGUAGE FlexibleContexts #-}

module Agent.PingPong.System.SendResult where

import AgentSystem.Generic
import Agent.PingPong
import Agent.PingPong.Role.SendResult hiding (runPingPong)

import Data.IORef

-----------------------------------------------------------------------------

modifyName i = unsafeModifyGenericRoleDescriptor
             $ \d -> d { agName = agName d ++ "-" ++ show i }

pingRole = flip modifyName pingRoleDescriptor
pongRole = flip modifyName pongRoleDescriptor

-----------------------------------------------------------------------------

runPingPong nPings = do putStrLn "<< Create System >> "
                        sys  <- newSimpleAgentSystem (return ())
                        let newPingPongPair i = do
                                  pingRef <- newIORef undefined
                                  pongRef <- newIORef undefined
                                  ping <- newAgentOfRole sys (pingRole i)
                                        $ return (nPings, pongRef)
                                  pong <- newAgentOfRole sys (pongRole i)
                                        $ return pingRef
                                  pingRef `writeIORef` someAgentRef ping
                                  pongRef `writeIORef` someAgentRef pong

                        putStrLn "Crateing agents"
                        mapM_ newPingPongPair [1..5]
                        putStrLn "Starting system"
                        startAllAgents sys

                        putStrLn "Collecting results:"
                        mapM print =<< awaitAllResults sys
                        putStrLn "Finished"
