-----------------------------------------------------------------------------
--
-- Module      :  Agent.Behavior
-- License     :  MIT
--
-- Maintainer  :  kdn.kovalev@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable (requires STM)
--
-----------------------------------------------------------------------------

module Agent.MsgResponse (

  MsgResponse(_msgResponse), respond

, ExpectingResponse, respondExpectingResponse, respondAwaitingResponse

, ConfirmMessage, AwaitingConfirmation, respondConfirm, respondCancel
, respondAwaitingConfirmation

) where

import Agent.Message

import Data.Maybe (fromMaybe)
import Data.Typeable (Typeable)

import Control.Applicative ( Alternative(..) )
import Control.Monad ( (<=<) )

-----------------------------------------------------------------------------

type RespondAndThen resp = (Response resp, IO ())

newtype MsgResponse resp = MsgResponse { _msgResponse :: IO (RespondAndThen resp) }


respond :: resp -> IO (MsgResponse resp)
respond = return . return



newMsgResponse x = MsgResponse $ return (x, return ())

instance Functor MsgResponse where
  fmap f resp = MsgResponse $ do (resp, after) <- _msgResponse resp
                                 return (f <$> resp, after)

instance Applicative MsgResponse where
  pure = newMsgResponse . return
  x <*> y = MsgResponse $ do
    (rf, af) <- _msgResponse x
    (rx, ax) <- _msgResponse y
    return (rf <*> rx, af >> ax)

instance Monad MsgResponse where
  resp >>= f = MsgResponse $ do
    (rx, ax) <- _msgResponse resp
    mbResp' <- mapM (_msgResponse . f) =<< waitResponse rx
    let (resp, after) = fromMaybe (empty, return ()) mbResp'
    return (resp, ax >> after)

instance Alternative MsgResponse where
  empty = newMsgResponse empty
  x <|> y = MsgResponse $ _msgResponse x <|> _msgResponse y

-----------------------------------------------------------------------------

type ExpectingResponse msg resp = (msg, Respond resp)

respondExpectingResponse :: msg -> (Response resp -> IO ())
                         -> MsgResponse (ExpectingResponse msg resp)
respondExpectingResponse msg handle = MsgResponse $
  do (promise, respond) <- promiseResponse
     return ( return (msg, respond), handle promise )

respondAwaitingResponse :: msg -> (Maybe resp -> IO ())
                        -> MsgResponse (ExpectingResponse msg resp)
respondAwaitingResponse msg handle = respondExpectingResponse msg
                                     (handle <=< waitResponse)

-----------------------------------------------------------------------------

data ConfirmMessage = ConfirmMessage deriving (Typeable, Show)
type AwaitingConfirmation msg = ExpectingResponse msg ConfirmMessage

respondAwaitingConfirmation :: msg -> IO () -> IO ()
                            -> MsgResponse (AwaitingConfirmation msg)
respondAwaitingConfirmation msg proceed cancel =
  respondAwaitingResponse msg handle
  where handle (Just ConfirmMessage) = proceed
        handle _                     = cancel

respondConfirm :: Respond ConfirmMessage -> IO ()
respondConfirm = provideResponse $ return ConfirmMessage

respondCancel :: Respond ConfirmMessage -> IO ()
respondCancel = provideResponse empty

-----------------------------------------------------------------------------
