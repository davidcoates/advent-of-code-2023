import Day4

import Data.Set (Set)
import qualified Data.Set as Set

cardWorth :: Card -> Int
cardWorth card = let n = cardMatches card in if n == 0 then 0 else 2 ^ (n - 1)

main :: IO ()
main = do
  cards <- parseCards
  let answer = sum $ map cardWorth cards
  print answer
