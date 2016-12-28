-----------------------------------------------------------------------------
--
-- Module      :  Agent.Ask
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  kdn.kovalev@gmail.com
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

{-# LANGUAGE ExistentialQuantification
           , Rank2Types
           , TypeFamilies
        #-}

module Agent.Ask (

  ResponseRef(..), AwaitingResponse(..)

, askConfirmating
, askConfirmatingAll
, respondConfirmating

, ConfirmOrCancel (..)

) where

import Agent.Abstract

import Control.Monad



data ConfirmOrCancel = Confirm | Cancel deriving (Show, Eq)




data ResponseRef msg = ResponseRef  { respond  :: msg -> IO () }
                     | ResponseRefT { respondT :: forall resp . resp ~ ExpectedResponse msg =>
                                                  msg -> IO resp
                                    }

newtype AwaitingResponse msg = AwaitingResponse (msg, ResponseRef (ExpectedResponse msg))

instance (Show msg) => Show (AwaitingResponse msg) where
    show (AwaitingResponse (msg,_)) = show msg



-- | Ask agents ('msg'), while 'cond' holds.
--   Send 'cancel' to those already sent to, if any fails.
--   Returns 'True' if all the agents have responded positively and 'False' otherwise.
askConfirmating :: ( Message msg, Message resp, Message cmsg
                   , ExpectedResponse msg ~ AwaitingResponse resp
                   , ExpectedResponse resp ~ cmsg
                   ) =>
                   [AgentRef] -> msg -> (resp -> Bool) -> cmsg -> cmsg -> IO Bool

askConfirmating [] _ _ _ _ = return False
askConfirmating refs msg cond confirm cancel
    = askConfirmating' refs [] msg cond confirm cancel

askConfirmating' (ref:rest) sent msg cond confirm cancel =
    do AwaitingResponse (resp, respf) <- ref `ask` msg
       if cond resp
          then askConfirmating' rest (respf:sent) msg cond confirm cancel
          else do forM_ sent (`respond` cancel)
                  return False

askConfirmating' [] sent _ _ _ confirm =
    do forM_ sent (`respond` confirm)
       return True


-- | Ask all agents ('msg'), and return the lists of confirmed
--   and rejected agents (depending on 'cond').
--   If any has rejected, send 'cancel' to all the confirmtion expecting agents.
askConfirmatingAll :: ( Message msg, Message resp, Message cmsg
                      , ExpectedResponse msg ~ AwaitingResponse resp
                      , ExpectedResponse resp ~ cmsg
                      ) =>
                      [AgentRef] -> msg -> (resp -> Bool) -> cmsg -> cmsg
                                 -> IO ([(AgentRef, resp)], [(AgentRef, resp)])

askConfirmatingAll refs msg cond confirm cancel =
    askConfirmatingAll' refs msg cond confirm cancel [] []

askConfirmatingAll' (ref:rest) msg cond confirm cancel accepted rejected =
    do  AwaitingResponse (resp, respf) <- ref `ask` msg
        let  r        = (ref, resp)
             (a', r') = if cond resp  then ((r, respf):accepted, rejected)
                                      else (accepted, r:rejected)
        askConfirmatingAll' rest msg cond confirm cancel a' r'

askConfirmatingAll' [] _ _ confirm cancel accepted rejected =
    let resp = if null rejected then confirm else cancel
    in do  forM_ accepted $ (`respond` resp) . snd
           return (fst <$> accepted, rejected)

respondConfirmating :: ( Message msg, Message resp, Message cmsg
                       , ExpectedResponse msg ~ AwaitingResponse resp
                       , ExpectedResponse resp ~ cmsg
                       ) => (msg -> IO resp)
                         -> (cmsg -> Bool)
                         -> (msg -> resp -> cmsg -> IO ())
                         -> msg
                         -> IO (AwaitingResponse resp)

respondConfirmating f isConfirmed onConfirm msg = do
    resp <- f msg
    return $ AwaitingResponse (resp, ResponseRef $
            \msg' -> when (isConfirmed msg') (onConfirm msg resp msg'))



