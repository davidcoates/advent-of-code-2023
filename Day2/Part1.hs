import Day2

import qualified Data.Map as Map

validDraw :: Draw -> Bool
validDraw draw = Red `atMost` 12 && Green `atMost` 13 && Blue `atMost` 14 where
  atMost color threshold = Map.findWithDefault 0 color draw <= threshold

main :: IO ()
main = do
  games <- parseGames
  let answer = sum [ id | (id, draws) <- games, all validDraw draws ]
  putStrLn $ show answer
