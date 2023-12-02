import Day2

import qualified Data.Map as Map

power :: Game -> Int
power (_, draws) = maxNum Red * maxNum Green * maxNum Blue where
  maxNum color = maximum $ Map.findWithDefault 0 color <$> draws

main :: IO ()
main = do
  games <- parseGames
  let answer = sum $ power <$> games
  putStrLn $ show answer
