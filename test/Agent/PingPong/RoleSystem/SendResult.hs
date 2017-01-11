module Agent.PingPong.RoleSystem.SendResult where

import AgentRoleSystem
import Agent.PingPong
import Agent.PingPong.Role.SendResult hiding (runPingPong)

import Data.IORef

-----------------------------------------------------------------------------

modifyName d i = modifyAgentDescriptorDescriptor d
               $ \d -> return d { agName = agName d ++ "-" ++ show i }

pingRole = modifyName pingRoleDescriptor
pongRole = modifyName pongRoleDescriptor

-----------------------------------------------------------------------------

runPingPong nPings = do putStrLn "<< Create Simple System >> "
                        sys  <- newRoleAgentSystem
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
