module Combinators where
    
import Control.Applicative
import Control.Arrow (second)

data Position = Position { line :: Integer, symb :: Integer }
data Stream s = Stream { pos :: Position, content :: s }

type ParseError = [String]

newtype Parser tok ok = Parser { runParser :: tok -> Either ParseError (tok, ok) }

instance Functor (Parser str) where
    fmap f p = Parser $ \s ->
      case runParser p s of
        Right (s', a) -> Right (s', f a)
        Left e -> Left e

instance Applicative (Parser str) where
    pure a = Parser $ \str -> Right (str, a)

    (Parser p) <*> (Parser q) = Parser $ \s -> do
        (s', ok') <- p s
        (s'', ok'') <- q s'
        pure (s'', ok' ok'')

instance Alternative (Parser str) where
    empty = Parser $ \_ -> empty
    p <|> q = Parser $ \s ->
      case runParser p s of
        Left _ -> runParser q s
        Right x -> Right x

instance Monad (Parser str) where
    return = pure
    (Parser p) >>= k = Parser $ \s -> do
      (s', ok') <- p s
      runParser (k ok') s'

instance Monoid err => Alternative (Either err) where
    empty = Left mempty
    Right a <|> _ = Right a
    Left _ <|> Right a = Right a
    Left e1 <|> Left e2 = Left $ e1 <> e2

token :: Eq token => token -> Parser [token] token
token t = Parser $ \s ->
  case s of
    (t' : s') | t == t' -> Right (s', t)
    _ -> Left ["Given token stream doesn't match."]

char :: Char -> Parser String Char
char = token

string :: String -> Parser String String
string [] = pure []
string (c:cs) = do
  char c
  string cs
  return (c:cs)

orChar :: [Char] -> Parser String Char
orChar [] = empty
orChar (c:cs) = char c <|> orChar cs

sepBy p sep = sepBy1 p sep <|> return []

sepBy1 p sep = do
  x <- p
  xs <- many (sep >> p)
  return (x:xs)

number :: Parser String Integer
number = read <$> some digit
  where
    digit = orChar ['0' .. '9']


spaces = many $ orChar spaceChars
spaceChars = ['\t', '\n', '\r', '\f', '\v', ' ']

betweenSpaces = between spaces

between b p = b *> p <* b
