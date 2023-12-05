{-# LANGUAGE NamedFieldPuns #-}

import Day5

mapImages :: Map -> Ranges -> Ranges
mapImages m = concatMap (mapImage m)

almanacImages :: Almanac -> Ranges
almanacImages Almanac{ seeds, maps } = chain (map mapImages maps) (seedRanges seeds) where
  seedRanges :: [Int] -> Ranges
  seedRanges [] = []
  seedRanges (x:y:ys) = (x, x + y - 1) : seedRanges ys

main :: IO ()
main = do
  almanac <- parseAlmanac
  let answer = minimum . map fst $ almanacImages almanac
  print $ answer
