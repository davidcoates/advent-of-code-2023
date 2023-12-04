import Day3

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)

gearPositions :: Grid -> Set Position
gearPositions = foldl (\ps (p, c) -> if c == '*' then Set.singleton p `Set.union` ps else ps) Set.empty

spanNeighbours :: Span -> Set Position
spanNeighbours ps = Set.unions [ neighbours p | p <- ps ] `Set.difference` Set.fromList ps

invertAL :: (Ord a, Ord b) => [(a, b)] -> Map b (Set a)
invertAL = foldl (\m (a, b) -> Map.insertWith Set.union b (Set.singleton a) m) Map.empty

-- Note: A number adjacent to * is a part number, so to find gears it suffices to find * symbols adjacent to exactly two numbers.
gears :: Grid -> [(Int, Int)]
gears grid = mapMaybe (pair . snd) $ Map.toList (invertAL links) where
  pair :: Set (Spanned Int) -> Maybe (Int, Int)
  pair s = case Set.toList s of
    [(_, a), (_, b)] -> Just (a, b)
    _                -> Nothing
  links :: [(Spanned Int, Position)]
  links = [ ((ps, n), p) | (ps, n) <- numbers grid, p <- Set.toList (spanNeighbours ps `Set.intersection` gearPositions grid) ]

main :: IO ()
main = do
  grid <- positioned <$> readFile "input.txt"
  let gearRatios = [ a * b | (a, b) <- gears grid ]
  print $ sum gearRatios
