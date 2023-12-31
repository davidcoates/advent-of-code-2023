{-# LANGUAGE NamedFieldPuns #-}

module Day4 where

import Parser

import Data.Set (Set)
import qualified Data.Set as Set

data Card = Card { winning :: Set Int, mine :: Set Int }

card :: Parser Card
card = (\winning mine -> Card { winning, mine }) <$> (matches "Card" *> some space *> int *> matches ":" *> ints) <*> (char '|' *> ints) where
  ints :: Parser (Set Int)
  ints = Set.fromList <$> (many space *> many (int <* many space))

cardMatches :: Card -> Int
cardMatches Card{ winning, mine } = length (winning `Set.intersection` mine)

parseCards :: IO [Card]
parseCards = map (forceParse card) . lines <$> readFile "input.txt"
