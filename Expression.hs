module Expression where

import Text.Printf
import ParserCombinators
import Combinators
import Control.Applicative ((<|>))

data Operator = Pow
              | Mul
              | Div
              | Sum
              | Minus
              | Eq
              | Neq
              | Le
              | Lt
              | Ge
              | Gt
              | Conj
              | Disj
  deriving Show
-- Simplest abstract syntax tree for expressions: only binops are allowed
data EAst a = BinOp Operator (EAst a) (EAst a)
            | Primary a
  deriving Show

cmpComb = string "=="
      <|> string "/="
      <|> string "<="
      <|> string "<"
      <|> string ">="
      <|> string ">"

operations = [(RAssoc, [(string "||", BinOp Disj)])
             ,(RAssoc, [(string "&&", BinOp Conj)])
             ,(NAssoc, [(string "==", BinOp Eq)
                       ,(string "/=", BinOp Neq)
                       ,(string "<=", BinOp Le)
                       ,(string "<",  BinOp Lt)
                       ,(string ">=", BinOp Ge)
                       ,(string ">",  BinOp Gt)])
             ,(LAssoc, [(string "+", BinOp Sum)
                       ,(string "-", BinOp Minus)])
             ,(LAssoc, [(string "*", BinOp Mul)
                       ,(string "/", BinOp Div)])
             ,(RAssoc, [(string "^", BinOp Pow)])]

primary = Primary <$> number

-- Change the signature if necessary
-- Constructs AST for the input expression
parseExpression :: String -> Either ParseError (EAst Integer)
parseExpression input = 
  runParserUntilEof (expression operations primary) input

-- Change the signature if necessary
-- Calculates the value of the input expression
executeExpression :: String -> Either String Integer
executeExpression input = eval <$> parseExpression input
  where
    eval (Primary a) = a
    eval (BinOp op e1 e2) = applyOp op (eval e1) (eval e2)

    applyOp Pow i1 i2 = i1 ^ i2
    applyOp Mul i1 i2 = i1 * i2
    applyOp Div i1 i2 = i1 `div` i2
    applyOp Sum i1 i2 = i1 + i2
    applyOp Minus i1 i2 = i1 - i2
    applyOp Eq i1 i2   = toInteger $ fromEnum $ i1  == i2
    applyOp Neq i1 i2  = toInteger $ fromEnum $ i1 /= i2
    applyOp Le i1 i2   = toInteger $ fromEnum $ i1 <= i2
    applyOp Lt i1 i2   = toInteger $ fromEnum $ i1 < i2
    applyOp Ge i1 i2   = toInteger $ fromEnum $ i1 >= i2
    applyOp Gt i1 i2   = toInteger $ fromEnum $ i1 > i2
    applyOp Conj i1 i2 = toInteger $ fromEnum $ (1 == i1) && (1 == i2)
    applyOp Disj i1 i2 = toInteger $ fromEnum $ (1 == i1) || (1 == i2)

instance Show Operator where
  show Pow   = "^"
  show Mul   = "*"
  show Div   = "/"
  show Sum   = "+"
  show Minus = "-"
  show Eq    = "=="
  show Neq   = "/="
  show Le    = "<="
  show Lt    = "<"
  show Ge    = ">="
  show Gt    = ">"
  show Conj  = "&&"
  show Disj  = "||"

instance Show a => Show (EAst a) where
  show = show' 0
    where
      show' n t =
        (if n > 0 then printf "%s|_%s" (concat (replicate (n - 1) "| ")) else id)
        (case t of
                  BinOp op l r -> printf "%s\n%s\n%s" (show op) (show' (ident n) l) (show' (ident n) r)
                  Primary x -> show x)
      ident = succ

{-
show (BinOp Conj (BinOp Pow (Primary 1) (BinOp Sum (Primary 2) (Primary 3))) (Primary 4))

&&
|_^
| |_1
| |_+
| | |_2
| | |_3
|_4
-}


test =
    let Right a1 = executeExpression "1+2+3"
        test1 = a1 == 6
        Right a2 = executeExpression "1+2*3"
        test2 = a2 == 7
        Right a3 = executeExpression "1*2+3"
        test3 = a3 == 5
        Right a4 = executeExpression "1^2^3"
        test4 = a4 == 1
        Right a5 = executeExpression "1&&0||1&&1"
        test5 = a5 == 1
        Right a6 = executeExpression "1||0&&0||1"
        test6 = a6 == 1
        Right a7 = executeExpression "10>4||10>=10"
        test7 = a7 == 1
    in and [test1, test2, test3, test4, test5, test6,test7]
