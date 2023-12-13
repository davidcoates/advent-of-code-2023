import Day12

main = do
  records <- parseRecords
  let answer = sum $ map (length . arrangements) records
  print answer
