{-# LANGUAGE NamedFieldPuns #-}

import Day6
import Parser

times :: Parser [Int]
times = matches "Time:" *> many (some space *> int)

records :: Parser [Int]
records = matches "Distance:" *> many (some space *> int)

races :: Parser [Race]
races = zipWith (\time record -> Race {time, record}) <$> (times <* newline) <*> records

parseRaces :: IO [Race]
parseRaces = do
  text <- readFile "input.txt"
  return $ forceParse races text

main :: IO ()
main = do
  races <- parseRaces
  let answer = foldl (*) 1 (map numWaysToWin races)
  print answer
