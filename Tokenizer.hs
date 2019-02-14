module Tokenizer where

import Combinators

data Token = Ident String
           | KeyWord String
           | Number Int  -- Change Number type if you work with something other than Int
           deriving (Show, Eq)

tokenize :: String -> [Token]
tokenize input = undefined

parseIdent :: Parser String String
parseIdent = undefined

parseKeyWord :: Parser String String
parseKeyWord = undefined

parseNumber :: Parser String Int
parseNumber = undefined
