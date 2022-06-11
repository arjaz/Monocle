{-# LANGUAGE OverloadedStrings #-}

module Server where

import           Control.Lens                   ( (^.) )
import           Monobank
import           Network.Wreq                   ( responseStatus
                                                , statusCode
                                                )
import           Web.Scotty

api :: ScottyM ()
api = userInfo <> statement

userInfo :: ScottyM ()
userInfo = get "/user-info/:token" $ do
  token <- param "token"
  info  <- liftAndCatchIO (clientInfo baseMonobankUrl token)
  case info of
    Left  err -> json (err ^. responseStatus . statusCode)
    Right i   -> json i

statement :: ScottyM ()
statement = get "/statement/:account-id/:from/:token" $ do
  accountId <- param "account-id"
  token     <- param "token"
  from      <- param "from"
  txs       <- liftAndCatchIO
    (accountStatement baseMonobankUrl token accountId from Nothing)
  case txs of
    Left  err -> json (err ^. responseStatus . statusCode)
    Right t   -> json t