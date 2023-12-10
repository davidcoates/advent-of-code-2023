import Day10

import Data.Array
import Data.Set (Set)
import qualified Data.Set as Set

main = do

  (start, grid) <- parseGrid
  let loop = traverseLoop start grid

  let loopBoundary :: Point -> Maybe (Edge, Edge)
      loopBoundary point
        -- Note: The edges of a cell both point outwards. The loop includes edge2, so edge1 must be flipped to point inwards (joining up with edge2)
        | Just edge2 <- point `lookup` loop = let [edge1] = filter (/= edge2) (edges (grid ! point)) in Just (coedge edge1, edge2)
        | otherwise                         = Nothing

      isInsideLoop :: Point -> Bool
      isInsideLoop point = if point `elem` map fst loop then False else isInsideLoop' point where
        isInsideLoop' point
          | not (bounds grid `inRange` next) = False
          | Just (edge1, edge2) <- loopBoundary next = (edge1 == east || edge2 == east) == isLoopClockwise loop
          | otherwise = isInsideLoop' next
          where
            next = point `step` north

  print $ length (filter isInsideLoop (indices grid))
