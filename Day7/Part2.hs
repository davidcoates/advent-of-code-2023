import Day7

replaceJoker :: Card -> Card -> Card
replaceJoker c J = c
replaceJoker _ c = c

replaceJokers :: Card -> [Card] -> [Card]
replaceJokers c = map (replaceJoker c)

jokerCompare :: Hand -> Hand -> Ordering
jokerCompare h1@(Hand cs1) h2@(Hand cs2) = (category (f h1), g cs1) `compare` (category (f h2), g cs2) where
  g :: [Card] -> [Card]
  g = replaceJokers (Number 0)
  f (Hand cs) = Hand (replaceJokers replacement cs) where
    replacement = let cs' = filter (/= J) cs in case cs' of
      [] -> J
      _  -> most cs'
    most :: Ord a => [a] -> a
    most = snd . last . counts

main = run jokerCompare
