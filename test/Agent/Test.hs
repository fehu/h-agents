-----------------------------------------------------------------------------
-- GenericAgent.Test
-----------------------------------------------------------------------------

module Main ( main ) where

import qualified Agent.Test.PingPongAgentsSend as PPASend
import qualified Agent.Test.PingPongAgentsAsk  as PPAAsk
import qualified Agent.Test.PingPongAgentsAsk2 as PPAAsk2

import Test.Hspec


maxCount = 10

main = hspec $
    describe "## PingPongAgents" $ do
        they "# shoud be able to communicate by sending messages"
            $ PPASend.testPingPong maxCount `shouldReturn` "Done"

        they "# shoud be able to communicate by asking"
            $ PPAAsk.testPingPong maxCount `shouldReturn` "Done"

        they "# shoud be able to communicate by asking (2)"
            $ PPAAsk2.testPingPong maxCount `shouldReturn` "Done"


they = it
