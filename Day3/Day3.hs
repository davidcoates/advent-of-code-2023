module Day3 where

import Data.Char (isDigit, digitToInt)
import Data.Set (Set)
import qualified Data.Set as Set

data Position = Position { row :: Int, col :: Int }
  deriving (Eq, Ord)

type Grid = [(Position, Char)]

type Span = [Position]

type Spanned a = (Span, a)

positioned :: String -> Grid
positioned grid = concat $ map (\(row, line) -> positioned' row line) (zip [0..] (lines grid)) where
  positioned' :: Int -> String -> Grid
  positioned' row line = zipWith (\col char -> let pos = Position { row = row, col = col } in (pos, char)) [0..] line

numbers :: Grid -> [Spanned Int]
numbers [] = []
numbers cs@((s, c):_) | isDigit c = let (d, cs') = stripNumber cs in d : numbers cs' where
  stripNumber :: Grid -> (Spanned Int, Grid)
  stripNumber cs = let (cs1, cs2) = span (\(_, c) -> isDigit c) cs
                       (ps, ds) = unzip cs1
                       in ((ps, read ds :: Int), cs2)
numbers (c:cs) = numbers cs

neighbours :: Position -> Set Position
neighbours p = Set.fromList [ Position { row = row p + r, col = col p + c } | r <- [-1, 0, 1], c <- [-1, 0, 1], r /= 0 || c /= 0 ]

isSymbol c = not (isDigit c) && c /= '.'

symbolNeighbours :: Grid -> Set Position
symbolNeighbours = foldl (\ps (p, c) -> if isSymbol c then neighbours p `Set.union` ps else ps) Set.empty

partNumbers :: Grid -> [Spanned Int]
partNumbers grid = [ (ps, n) | (ps, n) <- numbers grid, any (\p -> p `elem` symbolNeighbours grid) ps ]
