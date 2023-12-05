import Day5

lowestLocation :: Almanac -> Int
lowestLocation almanac = minimum (map (seedToLocation (maps almanac)) (seeds almanac)) where
  seedToLocation :: [Map] -> Int -> Int
  seedToLocation ms = chain (map mapLookup ms)

main :: IO ()
main = do
  almanac <- parseAlmanac
  print $ lowestLocation almanac
