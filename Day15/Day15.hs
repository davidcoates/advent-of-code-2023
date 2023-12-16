module Day15 where

import Data.List (foldl')
import Data.Char (ord)

hash :: String -> Int
hash = foldl' (\n c -> ((n + ord c) * 17) `rem` 256) 0

