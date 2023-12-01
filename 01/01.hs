import Data.List (isPrefixOf)
import Control.Monad (msum)

digitLiterals = [ ("0", 0), ("1", 1), ("2", 2), ("3", 3), ("4", 4), ("5", 5), ("6", 6), ("7", 7), ("8", 8), ("9", 9) ]
digitNames = [ ("one", 1), ("two", 2), ("three", 3), ("four", 4), ("five", 5), ("six", 6), ("seven", 7), ("eight", 8), ("nine", 9) ]

tryMatchPrefix :: [(String, Int)] -> String -> Maybe Int
tryMatchPrefix digits line = msum [ if name `isPrefixOf` line then Just value else Nothing | (name, value) <- digits ]

recover :: [(String, Int)] -> String -> Int
recover digits line = firstDigit * 10 + lastDigit where
    firstDigit = recover' digits line
    lastDigit  = recover' [ (reverse name, value) | (name, value) <- digits ] (reverse line)
    recover' digits line = case tryMatchPrefix digits line of
        Just value -> value
        Nothing    -> recover' digits (tail line)

part1 = digitLiterals
part2 = digitLiterals ++ digitNames

main = do
    lines <- lines <$> getContents
    let answer = sum $ map (recover part2) lines
    putStrLn $ show answer
