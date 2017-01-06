{-# LANGUAGE TypeFamilies #-}

module Agent.PingPong.Role.Send where

import AgentRole
import Agent.PingPong
import qualified Agent.PingPong.Simple.Send as Send

import Data.IORef

--------------------------------------------------------------------------------

data PingRole = PingRole
data PongRole = PongRole

--------------------------------------------------------------------------------

instance AgentRole PingRole where
  roleName _ = "Ping"

  type RoleState PingRole  = (IORef Integer, IORef SomeAgentRef, IORef Bool)
  type RoleResult PingRole = ()
  type RoleArgs PingRole   = (Integer, IORef SomeAgentRef)

instance AgentRole PongRole where
  roleName _ = "Pong"

  type RoleState PongRole  = IORef SomeAgentRef
  type RoleResult PongRole = ()
  type RoleArgs PongRole   = IORef SomeAgentRef

--------------------------------------------------------------------------------

pingRoleDescriptor = AgentRoleDescriptor PingRole
                    (return . uncurry Send.pingDescriptor)

pongRoleDescriptor = AgentRoleDescriptor PongRole
                    (return . Send.pongDescriptor)

--------------------------------------------------------------------------------

runPingPong nPings = do pingRef <- newIORef undefined
                        pongRef <- newIORef undefined

                        putStrLn "<< CreateAgentOfRole >> "
                        let pingC = CreateAgentOfRole pingRoleDescriptor
                                  $ return (nPings, pongRef)
                            pongC = CreateAgentOfRole pongRoleDescriptor
                                  $ return pingRef

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
