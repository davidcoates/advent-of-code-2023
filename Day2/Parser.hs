module Parser where

import Control.Applicative (Alternative (..))

newtype Parser a = Parser { runParser :: String -> (Maybe a, String) }

instance Functor Parser where
  fmap f p = Parser $ \cs -> let (r, cs') = runParser p cs in (f <$> r, cs')

instance Applicative Parser where
  pure x = Parser $ \cs -> (Just x, cs)
  (<*>) p1 p2 = Parser $ \cs -> let (r1, cs1) = runParser p1 cs in case r1 of
    Just r1 -> let (r2, cs2) = runParser p2 cs1 in case r2 of
      Just r2 -> (Just (r1 r2), cs2)
      Nothing -> (Nothing, cs2)
    Nothing -> (Nothing, cs1)

instance Alternative Parser where
  empty = Parser $ \cs -> (Nothing, cs)
  (<|>) p1 p2 = Parser $ \cs -> let (r1, cs1) = runParser p1 cs in case r1 of
    Just r1 -> (Just r1, cs1)
    Nothing -> runParser p2 cs1

match :: (Char -> Bool) -> Parser Char
match p = Parser $ \cs -> case cs of
  []     -> (Nothing, [])
  (c:cs) -> if p c then (Just c, cs) else (Nothing, c:cs)

satisfy :: (Char -> Bool) -> Parser ()
satisfy p = Parser $ \cs -> case cs of
  []     -> (Nothing, [])
  (c:cs) -> if p c then (Just (), c:cs) else (Nothing, c:cs)

consume :: Parser Char
consume = match (const True)

while :: (Char -> Bool) -> Parser b -> Parser [b]
while p t = (satisfy p *> ((:) <$> t <*> while p t)) <|> pure []

matches :: String -> Parser ()
matches []     = pure ()
matches (c:cs) = match (== c) *> matches cs

isDigit = (`elem` ['0'..'9'])
isLower = (`elem` ['a'..'z'])
isUpper = (`elem` ['A'..'Z'])
isLetter c = isLower c || isUpper c

int :: Parser Int
int = satisfy isDigit *> (read <$> while isDigit consume)

word :: Parser String
word = satisfy isLetter *> while isLetter consume

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p sep = (:) <$> p <*> (sep *> sepBy p sep <|> pure [])
