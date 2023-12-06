{-# LANGUAGE NamedFieldPuns #-}

import Day6
import Parser

badKerningInt :: Parser Int
badKerningInt = (read . concat) <$> many (some space *> some (match isDigit))

race :: Parser Race
race = (\time record -> Race {time, record}) <$> (time' <* newline) <*> record' where
  time' = matches "Time:" *> badKerningInt
  record' = matches "Distance:" *> badKerningInt

parseRace :: IO Race
parseRace = do
  text <- readFile "input.txt"
  return $ case runParser race text of
    (Just r, _) -> r

main :: IO ()
main = do
  race <- parseRace
  print $ numWaysToWin race
