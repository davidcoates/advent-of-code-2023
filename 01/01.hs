
main :: IO ()
main = do
    lines <- lines <$> getContents
    putStrLn (head lines)

