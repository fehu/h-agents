-----------------------------------------------------------------------------
--
-- Module      :  AgentSystem.Simple.Template
-- License     :  MIT
--
-- Maintainer  :  kdn.kovalev@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable (requires STM)
--
-----------------------------------------------------------------------------

module AgentSystem.Simple.Template(

  genAgentSystemInstances

, module Export

) where

import AgentSystem.Simple.Implementation as Export

import Data.Typeable

import Language.Haskell.TH

-----------------------------------------------------------------------------

genAgentSystemInstances :: String -> String -> DecsQ
genAgentSystemInstances sysName getRegistersName =
    mapM (($ (sysName, varE $ mkName getRegistersName)) . uncurry)
         [_genAgentsManagerInstance, _genAgentSystemInstance]

_genAgentsManagerInstance = _mkInstance "AgentsManager" "simpleAgentsManager"
                                        ["listAgents", "findAgent"]

_genAgentSystemInstance = _mkInstance "AgentSystem" "simpleAgentSystem"
                                      [ "listAgentsByRole", "listAgentsOfRoles"
                                      , "findAgentOfRole",  "newAgentOfRole" ]


_mkInstance className fpref fnames name registersExp =
  instanceD (cxt []) (conT (mkName className) `appT` conT (mkName name))
                     (_mkFun registersExp fpref <$> fnames)

_mkFun registersExp fpref fname = funD (mkName fname) [
    clause [] (normalB $ impl `appE` registersExp) []
   ]
  where impl = varE . mkName $ fpref ++ "_" ++ fname

-----------------------------------------------------------------------------
