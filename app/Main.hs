{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Maybe                     ( maybe )
import           Network.Wai.Middleware.Cors
import           Server
import           System.Environment             ( lookupEnv )
import           Web.Scotty

apiCors :: CorsResourcePolicy
apiCors = simpleCorsResourcePolicy
  { corsOrigins        = Nothing
  , corsMethods        = simpleMethods <> ["OPTIONS"]
  , corsRequestHeaders = simpleHeaders <> ["Content-Type", "Authorization"]
  }


main :: IO ()
main = do
  port <- maybe 3000 read <$> lookupEnv "PORT"
  scotty port $ do
    middleware (cors (const $ Just apiCors))
    api
