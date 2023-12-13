module Parser (
  module Parser,
  module Control.Applicative
) where

import Control.Applicative (Alternative (empty, (<|>)))

newtype Parser a = Parser { runParser :: String -> (Maybe a, String) }

forceParse :: Parser a -> String -> a
forceParse p text = case runParser p text of
  (Just r, []) -> r

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
isAlphaNum c = isLetter c || isDigit c

char :: Char -> Parser Char
char c = match (== c)

space :: Parser ()
space = char ' ' *> pure ()

comma :: Parser ()
comma = char ',' *> pure ()

newline :: Parser ()
newline = char '\n' *> pure ()

line :: Parser ()
line = many (match (/= '\n')) *> newline

int :: Parser Int
int = (char '-' *> (negate <$> int')) <|> int' where
  int' = satisfy isDigit *> (read <$> while isDigit consume)

integer :: Parser Integer
integer = satisfy isDigit *> (read <$> while isDigit consume)

word :: Parser String
word = satisfy isLetter *> while isLetter consume

many :: Parser a -> Parser [a]
many p = (:) <$> p <*> many p <|> pure []

some :: Parser a -> Parser [a]
some p = (:) <$> p <*> many p

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p sep = (:) <$> p <*> many (sep *> p)
