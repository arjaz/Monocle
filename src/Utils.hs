module Utils where

import           Control.Lens
import           Data.Char
import           Data.List
import           Data.Maybe

removePrefix :: String -> String -> String
removePrefix prefix = over _head toLower . fromJust . stripPrefix prefix
