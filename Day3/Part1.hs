import Day3

main :: IO ()
main = do
  grid <- positioned <$> readFile "input.txt"
  putStrLn . show . sum . map snd $ partNumbers grid
