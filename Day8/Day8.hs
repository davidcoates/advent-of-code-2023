{-# LANGUAGE NamedFieldPuns #-}

module Day8 where

import Parser
import Data.Map (Map)
import qualified Data.Map as Map

data Direction = L | R
  deriving Show

directions :: Parser [Direction]
directions = many direction where
  direction :: Parser Direction
  direction = (char 'L' *> pure L) <|> (char 'R' *> pure R)

type Node = String

node :: Parser Node
node = many (match isAlphaNum)

data Branch = Branch { source :: Node, left :: Node, right :: Node }

branch :: Parser Branch
branch = (\source left right -> Branch { source, left, right}) <$> (node <* matches " = ") <*> (matches "(" *> node <* matches ", ") <*> (node <* matches ")")

type Branches = Map Node (Node, Node)

type Network = ([Direction], Branches)

next :: Branches -> Direction -> Node -> Node
next branches direction node = let (left, right) = branches Map.! node in case direction of
  L -> left
  R -> right

parseNetwork :: IO Network
parseNetwork = do
  text <- readFile "input.txt"
  let (directionsLine : _ : branchLines) = lines text
      directions' = forceParse directions directionsLine
      branches = map (forceParse branch) branchLines
  return $ (directions', Map.fromList [ (source, (left, right)) | Branch{ source, left, right} <- branches ])
