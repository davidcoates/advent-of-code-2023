import Day12

import Data.List (intersperse)

main = do
  records <- parseRecords
  let records' = [ (concat (intersperse [Nothing] (replicate 5 conditions)), concat (replicate 5 damageds)) | (conditions, damageds) <- records ]
  let answer = sum $ map (length . arrangements) records'
  print answer
