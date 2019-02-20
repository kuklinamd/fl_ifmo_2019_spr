module Combinators where

import Prelude hiding (fail, seq)
import qualified Trie
import Control.Arrow (second)
import Control.Applicative (Alternative, empty, (<|>))

-- Parsing result is some payload and a suffix of the input which is yet to be parsed
newtype Parser str ok = Parser { runParser :: str -> Maybe (str, ok) }

-- Parser which always succeedes consuming no input
success :: ok -> Parser str ok
success ok = Parser $ \s -> Just (s, ok)

-- Parser which fails no mater the input
fail :: Parser str ok
fail = Parser $ const Nothing

-- Biased choice: if the first parser succeedes, the second is never run
-- Default sequence combinator
-- If the first parser succeedes then the second parser is used
-- If the first does not succeed then the second one is never tried
-- The result is collected into a pair
seq :: Parser str a -> Parser str b -> Parser str (a, b)
(Parser p) `seq` (Parser q) = Parser $ \s -> do
   (s', ok') <- p s
   (s'', ok'') <- q s'
   pure (s'', (ok', ok''))


instance Functor (Parser str) where
    -- Applies a function to the parsing result, if parser succeedes
    --fmap :: (a -> b) -> Parser str a -> Parser str b
    fmap f p = Parser $ \s ->
      case runParser p s of
        Just (s', a) -> Just (s', f a)
        _ -> Nothing

-- Monadic sequence combinator
instance Monad (Parser str) where
    return = pure
    --(>>=) :: Parser str a -> (a -> Parser str b) -> Parser str b
    (Parser p) >>= k = Parser $ \s -> do
      (s', ok') <- p s
      runParser (k ok') s'

instance Applicative (Parser str) where
    pure a = Parser $ \str -> Just (str, a)
    -- Applicative sequence combinator
    --(<*>) :: Parser str (a -> b) -> Parser str a -> Parser str b
    (Parser p) <*> (Parser q) = Parser $ \s -> do
        (s', ok') <- p s
        (s'', ok'') <- q s'
        pure (s'', ok' ok'')

instance Alternative (Parser str) where
    empty = Parser $ \_ -> Nothing
    --(<|>) :: Parser str ok -> Parser str ok -> Parser str ok
    p <|> q = Parser $ \s ->
      case runParser p s of
        Nothing -> runParser q s
        x -> x

-- Applies a parser once or more times
some :: Parser str a -> Parser str [a]
some p = some'
  where
    some' = (:) <$> p <*> many'
    many' = some' <|> (Parser $ \str -> Just (str, []))

-- Applies a parser zero or more times
many :: Parser str a -> Parser str [a]
many p = many'
  where
    some' = (:) <$> p <*> many'
    many' = some' <|> (Parser $ \str -> Just (str, []))

-- Parses keywords
keywords :: [String] -> Parser String String
keywords kws = Parser $ f (Trie.fromList kws)
  where
    f :: Trie.Trie -> String -> Maybe (String, String)
    f t s | Trie.isTerminal t = Just (s, "")
    f t [] = Nothing
    f t (c:cs) = (second (c:)) <$> (Trie.next t c >>= flip f cs)

-- Checks if the first element of the input is the given token
token :: Eq token => token -> Parser [token] token
token t = Parser $ \s ->
  case s of
    (t' : s') | t == t' -> Just (s', t)
    _ -> Nothing

-- Checks if the first character of the string is the one given
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
