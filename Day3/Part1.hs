import Day3

main :: IO ()
main = do
  grid <- positioned <$> readFile "input.txt"
  print . sum . map snd $ partNumbers grid
