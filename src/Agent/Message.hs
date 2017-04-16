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
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module Agent.Message(

  Message, ExpectedResponse, MessageResponse

, ResponseInterface(..), Response(..)
, ResponseProvider(..), Respond(..)

, ResponsePromise(..)

) where

import Data.Typeable (Typeable, Proxy(..))

import Control.Concurrent (forkIO)
import Control.Concurrent.STM ( STM, atomically
                              , TMVar, takeTMVar, putTMVar, newEmptyTMVarIO
                              )

import Control.Monad ( (<=<), void, forM, join )

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

type Message msg = (Typeable msg, Show msg)
type family ExpectedResponse msg :: *

type MessageResponse msg resp = (Message msg, Message resp, ExpectedResponse msg ~ resp)

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

class ResponseInterface promise where
  waitResponse        :: promise resp -> IO (Maybe resp)
  waitResponseSuccess :: promise resp -> IO resp
  waitResponses       :: Traversable t => t (promise resp) -> IO (Maybe (t resp))
  handleResponseAsync        :: promise resp -> (Maybe resp -> IO ()) -> IO ()
  handleResponseAsyncSuccess :: promise resp -> (      resp -> IO ()) -> IO ()

  waitResponses = fmap sequence . mapM waitResponse
    --  fmap sequence . atomically . sequence <=< mapM waitResponse'
  waitResponseSuccess = maybe noResponseFail return <=< waitResponse

  handleResponseAsync resp f = void . forkIO $ waitResponse resp >>= f
  handleResponseAsyncSuccess resp = handleResponseAsync resp
                                  . maybe noResponseFail

noResponseFail = fail "No response received"

-----------------------------------------------------------------------------

class ResponseProvider provider
  where
    provideResponse  :: Maybe resp -> provider resp -> IO ()
    respond          ::       resp -> provider resp -> IO ()
    dontRespond      ::               provider resp -> IO ()

    respond         = provideResponse . Just
    dontRespond     = provideResponse Nothing

-----------------------------------------------------------------------------

class ResponsePromise vower promise provider
  where
    promiseResponse :: vower -> IO (promise resp, provider resp)

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

newtype Response resp = Response { _responseWait  :: IO (Maybe resp) }

instance Functor Response where
  fmap f (Response wait) = Response $ fmap f <$> wait

instance Applicative Response where
  pure      = Response . return . Just
  rf <*> rx = Response $ do f' <- _responseWait rf
                            x' <- _responseWait rx
                            return $ f' <*> x'

instance Monad Response where
  rx >>= f = Response $ do mbResp <- _responseWait rx
                           fmap join . forM mbResp $ _responseWait . f

instance ResponseInterface Response where waitResponse = _responseWait

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

newtype Respond resp = Respond (TMVar (Maybe resp))

instance ResponseProvider Respond where
  provideResponse resp' (Respond var) = atomically $ putTMVar var resp'

-----------------------------------------------------------------------------
