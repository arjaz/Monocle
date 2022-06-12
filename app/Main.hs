module Main where

import           Data.Maybe                     ( maybe )
import           Server
import           System.Environment             ( lookupEnv )
import           Web.Scotty

main :: IO ()
main = do
  port <- maybe 3000 read <$> lookupEnv "PORT"
  scotty port api
