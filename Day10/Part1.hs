import Day10

main = do
  (start, grid) <- parseGrid
  let loop = traverseLoop start grid
  print $ length loop `div` 2
