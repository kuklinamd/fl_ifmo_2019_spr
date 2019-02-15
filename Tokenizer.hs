module Tokenizer (tokenize, Token(..)) where

import Data.Char (isDigit, digitToInt)
import Data.List (elem)
import qualified Numeric as N (readHex)

data Token = Ident String
           | KeyWord String
           | Number Int
           deriving (Show, Eq)

keywords :: [String]
keywords = ["return", "if", "else", "switch", "case", "int", "while"]

readHex :: String -> Int
readHex = readHex' . reverse
  where
    readHex' [] = 0
    readHex' (s:ss) = readHex' ss * 16 + digitToInt s

inside :: Char -> Char -> Char -> Bool
inside l r c = l <= c && c <= r

isOurChar :: Char -> Bool
isOurChar c = inside 'a' 'z' c || inside 'A' 'Z' c

isHexDigit :: Char -> Bool
isHexDigit c = isDigit c || inside 'a' 'f' c || inside 'A' 'F' c

isNonDigit :: Char -> Bool
isNonDigit c = (==) '_' c || isOurChar c

isIdent (c:cs) = isNonDigit c && isIdentNext cs
 where
  isIdentNext []     = True
  isIdentNext (c:cs) = (isDigit c || isNonDigit c) && isIdentNext cs
isIdent _ = False

isHexNumber (c1:c2:[]) = False
isHexNumber (c1:c2:cs) = c1 == '0' && (c2 == 'x' || c2 == 'X') && all isHexDigit cs
isHexNumber _ = False

toNum (c1:c2:cs) = readHex cs 

tokenize :: String -> [Token]
tokenize input = token <$> words input
 where
  token str | str `elem` keywords = KeyWord str
  token str | isIdent str         = Ident str
  token str | isHexNumber str     = Number $ toNum str
  token str = error $ "Word ``" ++ str ++ "'' is not our token"

testHexNumber =
     isHexNumber "10" == False
  && isHexNumber "0xZ" == False
  && isHexNumber "0x" == False
  && isHexNumber "0xffFF"
testNonDigit = all isNonDigit $ '_' : (['a' .. 'z'] ++ ['A' .. 'Z'])
testIdent =
     isIdent "a123"
  && isIdent "___"
  && isIdent "_123"
  && isIdent "12" == False
  && isIdent "#23" == False
testReadHex =
     readHex "1" == numericReadHex "1"
  && readHex "F" == numericReadHex "F"
  && readHex "FFF" == numericReadHex "FFF"
  && readHex "DEAD" == numericReadHex "DEAD"
  where numericReadHex = fst . head . N.readHex
