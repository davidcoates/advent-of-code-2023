import Day2

import qualified Data.Map as Map

power :: [Draw] -> Int
power draws = maxNum Red * maxNum Green * maxNum Blue where
  maxNum color = maximum $ Map.findWithDefault 0 color <$> draws

main :: IO ()
main = do
  games <- parseGames
  let answer = sum [ power draws | (_, draws) <- games ]
  putStrLn $ show answer
