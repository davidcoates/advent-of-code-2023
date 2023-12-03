-- module Day3 where

import Data.Char (isDigit, digitToInt)
import Data.Set (Set)
import qualified Data.Set as Set

data Position = Position { row :: Int, col :: Int }
  deriving (Eq, Ord)

type Positioned a = (Position, a)

type Sourced a = ([Position], a)

positioned :: String -> [Positioned Char]
positioned text = concat $ map (\(row, line) -> positioned' row line) (zip [0..] (lines text)) where
  positioned' :: Int -> String -> [Positioned Char]
  positioned' row line = zipWith (\col char -> let pos = Position { row = row, col = col } in (pos, char)) [0..] line

numbers :: [Positioned Char] -> [Sourced Int]
numbers [] = []
numbers cs@((s, c):_) | isDigit c = let (d, cs') = stripNumber cs in d : numbers cs' where
  stripNumber :: [Positioned Char] -> (Sourced Int, [Positioned Char])
  stripNumber cs = let (cs1, cs2) = span (\(_, c) -> isDigit c) cs
                       (ps, ds) = unzip cs1
                       in ((ps, read ds :: Int), cs2)
numbers (c:cs) = numbers cs

isSymbol c = not (isDigit c) && c /= '.'

symbolAdjacentPositions :: [Positioned Char] -> Set Position
symbolAdjacentPositions = foldl (\ps (p, c) -> if isSymbol c then adjacentPositions p `Set.union` ps else ps) Set.empty where
  adjacentPositions p = Set.fromList [ Position { row = row p + r, col = col p + c } | r <- [-1, 0, 1], c <- [-1, 0, 1] ]

main :: IO ()
main = do
  text <- positioned <$> readFile "input.txt"
  let adjacentNumbers = [ n | (ps, n) <- numbers text, any (\p -> p `elem` symbolAdjacentPositions text) ps ]
  putStrLn . show . sum $ adjacentNumbers
