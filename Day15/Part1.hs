import Day15

import Parser

parseHashes :: Parser [Int]
parseHashes = (hash <$> many (match (/= ','))) `sepBy` comma

main = do
  text <- readFile "input.txt"
  let hashes = forceParse parseHashes (filter (/= '\n') text)
  print $ sum hashes
