-----------------------------------------------------------------------------
--
-- Module      :  Agent.Message
-- License     :  MIT
--
-- Maintainer  :  kdn.kovalev@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable (requires STM)
--
-----------------------------------------------------------------------------

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Agent.Message(

  Message, Response(..), ExpectedResponse, MessageResponse
, handleResponseAsync, handleResponseSuccessAsync
, waitResponse, waitResponseSuccess

) where

import Data.Typeable (Typeable)

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.STM (atomically, TMVar, takeTMVar)

-----------------------------------------------------------------------------

type Message msg = (Typeable msg, Show msg)
newtype Response resp = Response (TMVar (Maybe resp))

type family ExpectedResponse msg :: *

type MessageResponse msg resp = (Message msg, Message resp, ExpectedResponse msg ~ resp)

-----------------------------------------------------------------------------

handleResponseAsync :: Response resp -> (Maybe resp -> IO ()) -> IO ()
handleResponseAsync resp f  = do
  thread <- forkIO $ waitResponse resp >>= f
  killThread thread

handleResponseSuccessAsync :: Response resp -> (resp -> IO ()) -> IO ()
handleResponseSuccessAsync resp f = handleResponseAsync resp f'
  where f' (Just r) = f r
        f' _        = fail "No response received"

waitResponse :: Response resp -> IO (Maybe resp)
waitResponse (Response respVar) = atomically $ takeTMVar respVar

waitResponseSuccess :: Response resp -> IO resp
waitResponseSuccess resp = do r <- waitResponse resp
                              case r of Just r' -> return r'
                                        _       -> fail "No response received"

-----------------------------------------------------------------------------
