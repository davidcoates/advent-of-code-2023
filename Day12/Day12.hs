module Day12 where

import Parser

data Condition = Damaged | Operational
  deriving Eq

instance Show Condition where
  show Operational = "."
  show Damaged = "#"

spreads :: Int -> Int -> [[Int]]
spreads n 1 = [[n]]
spreads n m = [ i : is | i <- [0..n], is <- spreads (n - i) (m - 1) ]

interleave :: [a] -> [a] -> [a]
interleave [x] [] = [x]
interleave (x:xs) (y:ys) = x : y : interleave xs ys

rawArrangements :: Int -> [Int] -> [[Condition]]
rawArrangements n damageds = [ arrangement gaps | extraGaps <- extraGapLists, let gaps = zipWith (+) minGaps extraGaps ] where
  minGaps = [0] ++ replicate (length damageds - 1) 1 ++ [0]
  extraGapLists = spreads (n - sum damageds - (length damageds - 1)) (length damageds + 1)
  arrangement :: [Int] -> [Condition]
  arrangement gaps = concat $ interleave (map (\n -> replicate n Operational) gaps) (map (\n -> replicate n Damaged) damageds)

type Record = ([Maybe Condition], [Int])

arrangements :: Record -> [[Condition]]
arrangements (targetConditions, damageds) = filter matches (rawArrangements (length targetConditions) damageds) where
  matches :: [Condition] -> Bool
  matches conditions = all (\(a, b) -> a == Nothing || a == Just b) $ zip targetConditions conditions

parseRecord :: String -> Record
parseRecord = forceParse record where
  record :: Parser Record
  record = (,) <$> many condition <*> (space *> (int `sepBy` comma))
  condition :: Parser (Maybe Condition)
  condition = (char '?' *> pure Nothing) <|> (char '#' *> pure (Just Damaged)) <|> (char '.' *> pure (Just Operational))

parseRecords :: IO [Record]
parseRecords = do
  text <- readFile "input.txt"
  return $ map parseRecord (lines text)
