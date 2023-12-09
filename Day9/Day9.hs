{-# LANGUAGE NamedFieldPuns #-}

module Day9 where

import Parser

type History = [Int]

difference :: History -> History
difference [_] = []
difference (x:y:ys) = (y-x) : difference (y:ys)

differences history = history : (if all (== 0) history then [] else differences (difference history))

parseHistory :: String -> History
parseHistory = forceParse history where
  history = int `sepBy` space

parseHistories :: IO [History]
parseHistories = do
  text <- readFile "input.txt"
  return $ map parseHistory (lines text)
