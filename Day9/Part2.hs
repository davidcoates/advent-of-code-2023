import Day9

main = do
  histories <- parseHistories
  print $ sum $ map extrapolate (map reverse histories)

