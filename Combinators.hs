module Combinators where
    
import Control.Applicative
import Control.Arrow (second)

data Position = Position { line :: Integer, symb :: Integer }

incSymb :: Position -> Position
incSymb (Position line symb) = Position line (succ symb)

incLine :: Position -> Position
incLine (Position line symb) = Position (succ line) symb

instance Show Position where
    show (Position l s) = "(" ++ show l ++ ", " ++ show s ++ ")"

data Stream s = Stream { pos :: Position, content :: s } deriving Show


uncons :: String -> Maybe Char
uncons [] = Nothing
uncons (c:cs) = Just c

type ParseError = [String]

newtype Parser tok ok = Parser { runStreamParser :: Stream tok -> Either ParseError (Stream tok, ok) }

runParser :: Parser tok ok -> tok -> Either ParseError (Stream tok, ok)
runParser p t = runStreamParser p (Stream (Position 0 0) t)

instance Functor (Parser str) where
    fmap f p = Parser $ \s ->
      case runStreamParser p s of
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
      case runStreamParser p s of
        Left _ -> runStreamParser q s
        Right x -> Right x

instance Monad (Parser str) where
    return = pure
    (Parser p) >>= k = Parser $ \s -> do
      (s', ok') <- p s
      runStreamParser (k ok') s'

instance Monoid err => Alternative (Either err) where
    empty = Left mempty
    Right a <|> _ = Right a
    Left _ <|> Right a = Right a
    Left e1 <|> Left e2 = Left $ e1 <> e2

char :: Char -> Parser String Char
char t = Parser $ \(Stream pos content) ->
  case content of
      (hd : tl) | hd == t -> Right (Stream (incPos hd pos) tl, hd)
      _ -> Left $ pure $ show pos ++ ": symbols doesn't match"
  where
    incPos '\n' = incLine . incSymb
    incPos _ = incSymb

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
