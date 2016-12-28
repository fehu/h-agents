-----------------------------------------------------------------------------
--
-- Module      :  Agent.Extra
-- Copyright   :
-- License     :  MIT
--
-- |
--
-----------------------------------------------------------------------------

module Agent.Extra (

  fromFullRef, extractThreads
, forceStopAgent, waitAgent

) where

import Agent.Abstract

import Control.Concurrent (killThread)
import Control.Monad (when)

_localDEBUG = True
localDebug scope = when _localDEBUG . putStrLn . (("[DEBUG] " ++ scope ++ ": ") ++)

-----------------------------------------------------------------------------

fromFullRef (AgentFullRef ref _) = AgentRef ref
extractThreads (AgentFullRef _ (AgentThreads act msg)) = (act, msg)


forceStopAgent :: AgentFullRef -> IO ()
forceStopAgent fref = do  _killThread act
                          _killThread msg
    where  (act, msg)   = extractThreads fref
           _killThread  = killThread . _threadId

waitAgent :: AgentFullRef -> IO ()
waitAgent fref = do localDebug "waitAgent" "_waitThread act"
                    _waitThread act
                    localDebug "waitAgent" "_waitThread msg"
                    _waitThread msg
    where  (act, msg)   = extractThreads fref
