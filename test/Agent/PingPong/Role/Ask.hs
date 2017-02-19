{-# LANGUAGE TypeFamilies #-}

module Agent.PingPong.Role.Ask where

import AgentSystem.Generic
import Agent.PingPong
import qualified Agent.PingPong.Simple.Ask as Ask

import Data.IORef

--------------------------------------------------------------------------------

data PingRole = PingRole
data PongRole = PongRole

--------------------------------------------------------------------------------

instance AgentRole PingRole where
  roleName _ = "Ping"

  type RoleState PingRole  = (IORef Integer, IORef SomeAgentRef)
  type RoleResult PingRole = ()
  type RoleArgs PingRole   = (Integer, IORef SomeAgentRef)

instance AgentRole PongRole where
  roleName _ = "Pong"

  type RoleState PongRole  = ()
  type RoleResult PongRole = ()
  type RoleArgs PongRole   = ()

--------------------------------------------------------------------------------

pingRoleDescriptor = genericRoleDescriptor PingRole
                    (return . uncurry Ask.pingDescriptor)

pongRoleDescriptor = genericRoleDescriptor PongRole
                    (const $ return Ask.pongDescriptor)

--------------------------------------------------------------------------------

runPingPong nPings = do pongRef <- newIORef undefined

                        putStrLn "<< CreateAgentOfRole >> "
                        let pingC = CreateAgentOfRole pingRoleDescriptor
                                  $ return (nPings, pongRef)
                            pongC = CreateAgentOfRole pongRoleDescriptor
                                  $ return ()

                        ping <- createAgentRef pingC
                        pong <- createAgentRef pongC

                        pongRef `writeIORef` someAgentRef pong

                        putStrLn "Starting PING"
                        agentStart ping
                        putStrLn "Starting PONG"
                        agentStart pong

                        putStrLn "Waiting PING termination"
                        agentWaitTermination ping
