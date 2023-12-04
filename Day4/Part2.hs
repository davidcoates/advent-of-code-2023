import Day4

import Data.Set (Set)
import qualified Data.Set as Set

winnings :: [Card] -> [Int]
winnings cards = map fst $ winnings' (zip (repeat 1) cards) where
  winnings' :: [(Int, Card)] -> [(Int, Card)]
  winnings' [] = []
  winnings' ((n, card):cards) = (n, card) : winnings' cards' where
    cards' = zipWith (\(n, card) m -> (n + m, card)) cards (replicate (cardMatches card) n ++ repeat 0)

main :: IO ()
main = do
  cards <- parseCards
  print $ sum (winnings cards)
