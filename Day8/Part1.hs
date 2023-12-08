import Day8

import qualified Data.Map as Map

steps :: Node -> Node -> [Direction] -> Branches -> Int
steps begin end (d:ds) branches
  | begin == end = 0
  | otherwise = 1 + steps (next branches d begin) end ds branches where

main = do
  (directions, branches) <- parseNetwork
  let numSteps = steps "AAA" "ZZZ" (cycle directions) branches
  print numSteps



