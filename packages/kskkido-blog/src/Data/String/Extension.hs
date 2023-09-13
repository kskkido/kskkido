module Data.String.Extension where

import RIO

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile (== ' ')

