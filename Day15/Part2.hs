{-# LANGUAGE NamedFieldPuns #-}

import Day15

import Parser
import Data.Array
import Data.List (foldl')

data Operation = Replace { label :: String, focalLength :: Int } | Remove { label :: String }
  deriving Show

parseOps :: Parser [Operation]
parseOps = parseOp `sepBy` comma where
  parseOp = mkOp <$> word <*> ((char '=' *> (Just <$> int)) <|> (char '-' *> pure Nothing))
  mkOp label            Nothing = Remove { label }
  mkOp label (Just focalLength) = Replace { label, focalLength }

type HashMap = Array Int [(String, Int)]

emptyHashMap :: HashMap
emptyHashMap = listArray (0, 255) [ [] | i <- [0..255] ]

replace :: Eq a => (a, b) -> [(a, b)] -> [(a, b)]
replace (k, v) ls = case ls of
  []            -> [(k, v)]
  ((k', v'):ls)
    | k == k'   -> (k, v) : ls
    | otherwise -> (k', v') : replace (k, v) ls

apply :: HashMap -> Operation -> HashMap
apply hashMap op = let h = hash (label op) in hashMap // [(h, applyBox (hashMap ! h))] where
  applyBox box
    | Replace { label, focalLength } <- op
      = replace (label, focalLength) box
    | Remove { label } <- op
      = filter (\(k, _) -> k /= label) box

focusingPower :: HashMap -> Int
focusingPower hashMap = sum $ map fpBox (assocs hashMap) where
  fpBox :: (Int, [(String, Int)]) -> Int
  fpBox (box, lenses) = (box + 1) * sum (zipWith (\slot (_, fl) -> slot * fl) [1..] lenses)

main = do
  text <- readFile "input.txt"
  let ops = forceParse parseOps (filter (/= '\n') text)
  let hashMap = foldl' apply emptyHashMap ops
  print $ focusingPower hashMap
