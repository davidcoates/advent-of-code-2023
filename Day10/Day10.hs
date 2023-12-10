module Day10 where

import Parser
import Data.Array

data Cell = VPipe | HPipe | NEBend | NWBend | SWBend | SEBend | Ground | Start
  deriving (Show, Eq)

type Edge = (Int, Int)

north = (-1, 0)
east = (0, 1)
south = (1, 0)
west = (0, -1)

right :: Edge -> Edge
right edge = case edge `lookup` [(north, east), (east, south), (south, west), (west, north)] of
  Just edge -> edge

left :: Edge -> Edge
left edge = case edge `lookup` [(north, west), (west, south), (south, east), (east, north)] of
  Just edge -> edge

invert :: Edge -> Edge
invert = right . right

edges :: Cell -> [Edge]
edges cell = case cell of
  VPipe -> [north, south]
  HPipe -> [east, west]
  NEBend -> [north, east]
  NWBend -> [north, west]
  SWBend -> [south, west]
  SEBend -> [south, east]
  _ -> []

coedge :: Edge -> Edge
coedge (x, y) = (-x, -y)

type Point = (Int, Int)
step :: Point -> Edge -> Point
step (x, y) (i, j) = (x + i, y + j)

cell :: Parser Cell
cell = (char '|' *> pure VPipe) <|>
       (char '-' *> pure HPipe) <|>
       (char 'L' *> pure NEBend) <|>
       (char 'J' *> pure NWBend) <|>
       (char '7' *> pure SWBend) <|>
       (char 'F' *> pure SEBend) <|>
       (char '.' *> pure Ground) <|>
       (char 'S' *> pure Start)

grid :: Parser [[Cell]]
grid = init <$> many cell `sepBy` newline

type Grid = Array (Int, Int) Cell

mkGrid :: [[Cell]] -> Grid
mkGrid rows = array ((1, 1), (h, w)) [ ((x, y), cell) | (x, row) <- zip [1..] rows, (y, cell) <- zip [1..] row ]
  where
    w = length (head rows)
    h = length rows

removeStart :: Grid -> ((Int, Int), Grid)
removeStart grid = (start, grid // [(start, pipe)]) where
  [start] = [ pos | (pos, value) <- assocs grid, value == Start ]
  points edge = bounds grid `inRange` neighbour && (invert edge) `elem` edges (grid ! neighbour)
    where neighbour = start `step` edge
  -- TODO could do this in a nicer way by inverting "edges"
  pipe = case (points north, points west, points south, points east) of
    (True, False, True, False) -> VPipe
    (False, True, False, True) -> HPipe
    (True, True, False, False) -> NWBend
    (False, False, True, True) -> SEBend
    (True, False, False, True) -> NEBend
    (False, True, True, False) -> SWBend

parseGrid :: IO (Point, Grid)
parseGrid = do
  text <- readFile "input.txt"
  return $ removeStart . mkGrid $ forceParse grid text

traverseLoop :: Point -> Grid -> [(Point, Edge)]
traverseLoop start grid = let edge = head (edges (grid ! start)) in traverseLoop' start edge where
  traverseLoop' :: (Int, Int) -> Edge -> [(Point, Edge)]
  traverseLoop' point edge = (point, edge) : (if nextPoint == start then [] else traverseLoop' nextPoint nextEdge) where
    nextPoint = point `step` edge
    [nextEdge] = filter (/= coedge edge) $ edges (grid ! nextPoint)

isLoopClockwise :: [(Point, Edge)] -> Bool
isLoopClockwise path = netTurnQuality edges > 0 where -- We expect 4 right turns (+4) or 4 left turns (-4) (might be one off if the start is on a bend)
  edges = map snd path
  netTurnQuality :: [Edge] -> Int
  netTurnQuality (e1:e2:es) = turnQuality (e1, e2) + netTurnQuality (e2:es)
  netTurnQuality _ = 0
  turnQuality :: (Edge, Edge) -> Int
  turnQuality (e1, e2)
    | e2 == right e1 = 1
    | e2 == left e1 = -1
    | otherwise = 0
    where
      rightTurns = [ (north, east), (east, south), (south, west), (west, north) ]

