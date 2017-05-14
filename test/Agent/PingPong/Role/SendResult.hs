{-# LANGUAGE TypeFamilies #-}

module Agent.PingPong.Role.SendResult where

import AgentSystem.Generic
import Agent.PingPong
import qualified Agent.PingPong.Simple.SendResult as SendRes

import Data.IORef

--------------------------------------------------------------------------------

data PingRole = PingRole
data PongRole = PongRole

--------------------------------------------------------------------------------

instance RoleName PingRole where roleName _ = "Ping"
instance AgentRole PingRole where
  type RoleState PingRole  = (IORef Integer, IORef SomeAgentRef, IORef Bool)
  type RoleResult PingRole = ()
  type RoleSysArgs PingRole = ()
  type RoleArgs PingRole   = (Integer, IORef SomeAgentRef)

instance RoleName PongRole where roleName _ = "Pong"
instance AgentRole PongRole where
  type RoleState PongRole  = (IORef Integer, IORef SomeAgentRef)
  type RoleResult PongRole = Integer
  type RoleSysArgs PongRole = ()
  type RoleArgs PongRole   = IORef SomeAgentRef

--------------------------------------------------------------------------------

pingRoleDescriptor = genericRoleDescriptor PingRole
                   (const $ return . uncurry SendRes.pingDescriptor)

pongRoleDescriptor =  genericRoleDescriptor PongRole
                   (const $ return . SendRes.pongDescriptor)

--------------------------------------------------------------------------------

runPingPong nPings = do pingRef <- newIORef undefined
                        pongRef <- newIORef undefined

                        putStrLn "<< CreateAgentOfRole >> "
                        let pingC = CreateAgentOfRole pingRoleDescriptor
                                  (return ()) $ return (nPings, pongRef)
                            pongC = CreateAgentOfRole pongRoleDescriptor
                                  (return ()) $ return pingRef

                        ping <- createAgentRef pingC
                        pong <- createAgentRef pongC

                        pingRef `writeIORef` someAgentRef ping
                        pongRef `writeIORef` someAgentRef pong

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
