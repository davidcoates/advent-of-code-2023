import Day11

main = do
  text <- readFile "input.txt"
  let galaxies = lines text
  print $ distances galaxies 2
