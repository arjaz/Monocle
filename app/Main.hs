module Main where

import           Server
import           Web.Scotty

main :: IO ()
main = scotty 3000 api
