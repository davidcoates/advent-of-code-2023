module Day2 where

import Parser

import Data.Tuple (swap)
import Data.Map (Map)
import qualified Data.Map as Map

data Color = Red | Green | Blue
  deriving (Eq, Ord, Show)

color :: Parser Color
color = mapColor <$> word where
  mapColor "red"   = Red
  mapColor "green" = Green
  mapColor "blue"  = Blue

type Draw = Map Color Int

draw :: Parser Draw
draw = Map.fromList <$> ((\n c -> (c, n)) <$> int <*> (matches " " *> color)) `sepBy` matches ", "

type Game = (Int, [Draw])

game :: Parser Game
game = (,) <$> (matches "Game " *> int <* matches ": ") <*> draw `sepBy` matches "; "

parseGames :: IO [Game]
parseGames = map (forceParse game) . lines <$> readFile "input.txt"
