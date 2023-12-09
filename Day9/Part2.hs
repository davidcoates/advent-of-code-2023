import Day9

extrapolate history = extrapolate' (reverse (map head (differences history))) 0 where
  extrapolate' [] m = m
  extrapolate' (n:ns) m = extrapolate' ns (n - m)

main = do
  histories <- parseHistories
  print $ sum $ map extrapolate histories

