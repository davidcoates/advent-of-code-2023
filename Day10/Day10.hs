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

points :: Cell -> Edge -> Bool
points cell edge = edge `elem` edges cell

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
mkGrid rows = array ((1, 1), (w, h)) [ ((x, y), cell) | (x, row) <- zip [1..] rows, (y, cell) <- zip [1..] row ]
  where
    w = length (head rows)
    h = length rows

removeStart :: Grid -> ((Int, Int), Grid)
removeStart grid = (start, grid // [(start, pipe)]) where
  [start] = [ pos | (pos, value) <- assocs grid, value == Start ]
  -- TODO could do this in a nicer way by inverting "edges"
  pipe = case (grid ! (start `step` north) `points` south, grid ! (start `step` west) `points` east, grid ! (start `step` south) `points` north, grid ! (start `step` east) `points` west) of
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

loopLength :: Point -> Grid -> Int
loopLength start grid = let edge = head (edges (grid ! start)) in loopLength' start edge where
  loopLength' :: (Int, Int) -> Edge -> Int
  loopLength' point edge = 1 + (if nextPoint == start then 0 else loopLength' nextPoint nextEdge) where
    nextPoint = point `step` edge
    [nextEdge] = filter (/= coedge edge) $ edges (grid ! nextPoint)

