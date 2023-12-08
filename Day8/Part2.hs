import Day8

import qualified Data.Map as Map

isStarting node = last node == 'A'
isEnding node = last node == 'Z'

steps :: Node -> [Direction] -> Branches -> Int
steps node (d:ds) branches
  | isEnding node = 0
  | otherwise = 1 + steps (next branches d node) ds branches


main = do
  (directions, branches) <- parseNetwork
  let numSteps = foldl lcm 1 numStepsPerNode
      numStepsPerNode = map (\node -> steps node (cycle directions) branches) (filter isStarting (Map.keys branches))
  print numSteps



