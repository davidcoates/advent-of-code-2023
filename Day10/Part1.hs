import Day10

main = do
  (start, grid) <- parseGrid
  print $ loopLength start grid `div` 2

