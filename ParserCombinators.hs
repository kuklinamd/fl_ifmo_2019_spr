module ParserCombinators
      --( Parser(..)
      --, ParseError
      --, runParser
      --, betweenSpaces
      --, char
      --, orChar
      --, some
      --, many
      --, sepBy
      --, eof
      --, string
      --, number)
      where
        
import Control.Applicative hiding (many, some)
import qualified Control.Applicative as CA (many, some)
import Control.Arrow (second)

data Position = Position { line :: Integer, symb :: Integer }

incSymb :: Position -> Position
incSymb (Position line symb) = Position line (succ symb)

incLine :: Position -> Position
incLine (Position line symb) = Position (succ line) symb

instance Show Position where
    show (Position l s) = "(" ++ show l ++ ", " ++ show s ++ ")"

data Stream s = Stream { pos :: Position, content :: s } deriving Show

isEmpty :: Foldable f => Stream (f s) -> Bool
isEmpty (Stream p s) = null s

uncons :: String -> Maybe Char
uncons [] = Nothing
uncons (c:cs) = Just c

type ParseError = String

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
      _ -> Left $ "char: " ++ show pos ++ ": symbol" ++ printU (uncons content) ++ " doesn't match " ++ show t
  where
    incPos '\n' = incLine . incSymb
    incPos _ = incSymb

printU = maybe "" show

string :: String -> Parser String String
string [] = pure []
string (c:cs) = do
  char c
  string cs
  return (c:cs)

orChar :: [Char] -> Parser String Char
orChar [] = Parser $ \(Stream pos content) -> Left $ "orChar: " ++ show pos ++
                                                     ": symbol '" ++ printU (uncons content) ++
                                                     "' doesn't match any char from the given list."
orChar (c:cs) = char c <|> orChar cs

sepBy p sep = sepBy1 p sep <|> return []

sepBy1 p sep = do
  x <- p
  xs <- many (sep >> p)
  return (x:xs)

number :: Parser String Integer
number = read <$> stringNumber

digit = orChar ['0' .. '9']

stringNumber = some digit

ident :: Parser String String
ident = do
    c <- orChar identCharsStart
    ss <- many (orChar identChars)
    pure (c:ss)

identCharsStart = '_' : (['a'..'z'] ++ ['A'..'Z'])
identChars = ['0' .. '9'] ++ identCharsStart

spaces1 = some $ orChar spaceChars
spaces = many $ orChar spaceChars
spaceChars = ['\t', '\n', '\r', '\f', '\v', ' ']

betweenSpaces = between spaces

between b p = b *> p <* b

eof :: Parser String ()
eof = Parser $ \s -> if null (content s)
                     then Right (s, ())
                     else Left $ "eof: " ++ show (pos s) ++ " symbol '" ++ printU (uncons $ content s) ++ "' isn't EOF symbol"

nullify = fmap (const ())

word p = p <* (nullify eof <|> nullify spaces1)

many = CA.many
some = CA.some
