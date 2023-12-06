{-# LANGUAGE NamedFieldPuns #-}

module Day6 where

data Race = Race { time :: Int, record :: Int }

numWaysToWin :: Race -> Int
numWaysToWin race@Race{ time, record } = length [ hold | hold <- [0..time], hold * (time - hold) > record ]
