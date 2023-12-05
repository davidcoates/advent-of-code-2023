{-# LANGUAGE NamedFieldPuns #-}

module Day5 where

import Data.List (sortBy)
import Parser

data Segment = Segment { dstBegin :: Int, srcBegin :: Int, len :: Int }

data Map = Map { segments :: [Segment] }

mkMap :: [Segment] -> Map
mkMap segments = Map { segments = sortBy (\a b -> srcBegin a `compare` srcBegin b) segments }

type Range = (Int, Int)

type Ranges = [Range]

mapImage :: Map -> Range -> Ranges
mapImage Map{ segments } (a, b) = mapImage' segments (a, b) where
  mapImage' segments (a, b) = case segments of
    [] -> [(a, b)]
    (Segment{ dstBegin, srcBegin, len }:segments)
      | b < srcBegin -> mapImage' segments (a, b)
      | a > srcEnd   -> mapImage' segments (a, b)
      | otherwise    -> [ (max srcBegin a + shift, min srcEnd b + shift) ] ++ leftSplit ++ rightSplit
      where
        srcEnd = srcBegin + len - 1
        shift = dstBegin - srcBegin
        leftSplit = if a < srcBegin then mapImage' segments (a, srcBegin) else []
        rightSplit = if b > srcEnd then mapImage' segments (srcEnd, b) else []

mapLookup :: Map -> Int -> Int
mapLookup m n = case mapImage m (n, n) of
  [(a, b)] | a == b -> a

chain :: [a -> a] -> a -> a
chain [f] x = f x
chain (f:fs) x = chain fs (f x)

data Almanac = Almanac { seeds :: [Int], maps :: [Map] }

almanac :: Parser Almanac
almanac = (\seeds maps -> Almanac { seeds, maps }) <$> (matches "seeds: " *> int `sepBy` space <* newline) <*> many map where
  map :: Parser Map
  map = newline *> line *> (mkMap <$> many segment)
  segment :: Parser Segment
  segment = (\dstBegin srcBegin len -> Segment { dstBegin, srcBegin, len }) <$> (int <* space) <*> (int <* space) <*> (int <* newline)

parseAlmanac :: IO Almanac
parseAlmanac = do
  text <- readFile "input.txt"
  return $ case runParser almanac text of
    (Just r, _) -> r
