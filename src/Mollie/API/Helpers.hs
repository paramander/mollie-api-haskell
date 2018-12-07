module Mollie.API.Helpers where

import           Data.Char (toLower)

lowerFirst :: String -> String
lowerFirst []     = []
lowerFirst (x:xs) = toLower x : xs
