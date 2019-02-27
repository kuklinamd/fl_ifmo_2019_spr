module Tokenizer where

import Combinators
import Control.Applicative ((<|>))
import Data.Char (digitToInt)

data Token = Ident String
           | KeyWord String
           | Number Integer  -- Change Number type if you work with something other than Int
           deriving (Show, Eq)

digits = ['0' .. '9']
hexDigits = digits ++ ['a' .. 'f'] ++ ['A' .. 'F']
nonDigits = '_' : (['a'..'z'] ++ ['A' .. 'Z'])
kwds = ["return", "if", "else", "switch", "case", "int", "while", "where"]

tokenize :: String -> [Token]
tokenize = maybe [] id . sequence . fmap tokenize' . words
  where
    tokenize' :: String -> Maybe Token
    tokenize' s = snd <$> runParser (number <|> keyword <|> ident) s

number :: Parser String Token
number = Parser $ \s -> case runParser parseNumber s of {Just ([],s) -> Just ([], Number s); _ -> Nothing}

keyword :: Parser String Token
keyword = Parser $ \s -> case runParser parseKeyWord s of {Just ([],s) -> Just ([], KeyWord s); _ -> Nothing}

ident :: Parser String Token
ident = Parser $ \s -> case runParser parseIdent s of {Just ([],s) -> Just ([], Ident s); _ -> Nothing}

-- indent = nondigit (digit | nondigit)*
parseIdent :: Parser String String
parseIdent = do
  nd <- nondigit
  other <- many (nondigit <|> digit)
  return (nd:other)
  where
    nondigit = orChar nonDigits
    digit = orChar digits

parseKeyWord :: Parser String String
parseKeyWord = keywords kwds

-- number = (0x | 0X) digit+
parseNumber :: Parser String Integer
parseNumber = readHex <$> ((string "0x" <|> string "0X") *> some hexDigit)
  where
    readHex :: String -> Integer
    readHex = readHex' . reverse
      where
        readHex' [] = 0
        readHex' (s:ss) = readHex' ss * 16 + toInteger (digitToInt s)

    hexDigit :: Parser String Char
    hexDigit = orChar hexDigits
