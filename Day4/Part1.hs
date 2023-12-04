import Day4

import Data.Set (Set)
import qualified Data.Set as Set

cardWorth :: Card -> Int
cardWorth Card{ winning, mine } = if matches == 0 then 0 else 2 ^ (matches - 1) where
  matches = length (winning `Set.intersection` mine)

main :: IO ()
main = do
  cards <- parseCards
  let answer = sum $ map cardWorth cards
  putStrLn (show answer)
