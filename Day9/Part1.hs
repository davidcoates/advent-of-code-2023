import Day9

extrapolate history = sum $ map last (differences history)

main = do
  histories <- parseHistories
  print $ sum $ map extrapolate histories

