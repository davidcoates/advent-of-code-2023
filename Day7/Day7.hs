module Day7 where

import Parser
import Data.List (sort, sortBy)

data Card = Number Int | T | J | Q | K | A
  deriving (Ord, Eq, Show)

newtype Hand = Hand [Card]

instance Eq Hand where
  (==) (Hand cs1) (Hand cs2) = cs1 == cs2

data Category = HighCard | OnePair | TwoPair | ThreeOfAKind | FullHouse | FourOfAKind | FiveOfAKind
  deriving (Ord, Eq)

counts :: Ord a => [a] -> [(Integer, a)]
counts cards = sort (count' (sort cards)) where
  count' [] = []
  count' [a] = [(1, a)]
  count' (a:b:bs)
    | a == b = let (n, _):ns = count' (b:bs) in (n+1, a) : ns
    | otherwise = (1, a) : count' (b:bs)

category :: Hand -> Category
category (Hand cards) = case map fst (counts cards) of
  [5]        -> FiveOfAKind
  [1,4]      -> FourOfAKind
  [2,3]      -> FullHouse
  [1,1,3]    -> ThreeOfAKind
  [1,2,2]    -> TwoPair
  [1,1,1,2]  -> OnePair
  [1,1,1,1,1]-> HighCard

card :: Parser Card
card = (char 'T' *> pure T) <|>
       (char 'J' *> pure J) <|>
       (char 'Q' *> pure Q) <|>
       (char 'K' *> pure K) <|>
       (char 'A' *> pure A) <|>
       ((Number . read . (:[])) <$> match isDigit)

hand :: Parser (Hand, Integer)
hand = (,) <$> (Hand <$> many card) <*> (space *> integer)

parseHands :: IO [(Hand, Integer)]
parseHands = do
  text <- readFile "input.txt"
  return $ map parseHand (lines text) where
    parseHand :: String -> (Hand, Integer)
    parseHand line = case runParser hand line of
      (Just r, []) -> r

run :: (Hand -> Hand -> Ordering) -> IO ()
run compare = do
  hands <- parseHands
  let sortedHands = sortBy (\a b -> fst a `compare` fst b) hands
      sortedBids = map snd sortedHands
      answer = sum $ zipWith (\bid rank -> bid * rank) sortedBids [1..]
  print answer
