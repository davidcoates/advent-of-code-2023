module Day11 where

import Data.Array
import Data.List (transpose)

toArray :: [a] -> Array Int a
toArray list = listArray (1, length list) list

distances :: [String] -> Int -> Int
distances galaxies expansionFactor = sum [ distance p1 p2 | p1 <- points, p2 <- points, p1 < p2 ] where
  points = [ (r, c) | (r, row) <- zip [1..] galaxies, (c, cell) <- zip [1..] row, cell == '#' ]
  rowWidths = toArray $ map (\row -> if all (== '.') row then expansionFactor else 1) galaxies
  colWidths = toArray $ map (\row -> if all (== '.') row then expansionFactor else 1) (transpose galaxies)
  distance (x1, y1) (x2, y2) = distance' rowWidths x1 x2 + distance' colWidths y1 y2 where
    distance' widths x1 x2
      | x1 == x2  = 0
      | x1 < x2   = (widths ! x1) + distance' widths (x1 + 1) x2
      | otherwise = distance' widths x2 x1

