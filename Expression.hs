module Expression (parseExpression) where

import Text.Printf
import Combinators
import Control.Applicative (many, (<|>))

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
  deriving (Eq)

-- Simplest abstract syntax tree for expressions: only binops are allowed
data EAst a = BinOp Operator (EAst a) (EAst a)
            | Primary a
  deriving (Eq)

-- Change the signature if necessary
parseExpression :: String -> Either ParseError (EAst Integer)
parseExpression input = snd <$> runParser (exprP <* eof) input

binOp = flip BinOp

exprP =  orP

orP = (binOp <$> andP <*> orOp <*> orP) <|> andP

andP =(binOp <$> cmpP <*> andOp <*> andP) <|> cmpP

cmpP = (binOp <$> plusMinP <*> cmpOp <*> plusMinP) <|> plusMinP

plusMinP :: Parser String (EAst Integer)
plusMinP = ((\ei f -> f ei) <$> mulDivP <*> plusMinP') <|> mulDivP
  where
    plusMinP' :: Parser String (EAst Integer -> EAst Integer)
    plusMinP' =
        (do
        op <- plusMinOp
        ex <- mulDivP
        f  <- plusMinP'
        pure (BinOp op (f ex))
        )
        <|>
        (do
        op <- plusMinOp
        ex <- mulDivP
        pure (BinOp op ex))

mulDivP :: Parser String (EAst Integer)
mulDivP = ((\ei f -> f ei) <$> powP <*> mulDivP') <|> powP
  where
    mulDivP' :: Parser String (EAst Integer -> EAst Integer)
    mulDivP' =
        (do
        op <- mulDivOp
        ex <- powP
        f  <- mulDivP'
        pure (BinOp op (f ex))
        )
        <|>
        (do
        op <- mulDivOp
        ex <- powP
        pure (BinOp op ex))

powP :: Parser String (EAst Integer)
powP = (binOp <$> atomP <*> powOp <*> powP) <|> atomP

atomP = (Primary <$> number) <|> (char '(' *> exprP <* char ')')

andOp = andCtr <$> string "&&"
  where
    andCtr _ = Conj

orOp = orCtr <$> string "||"
  where
    orCtr _ = Disj

plusMinOp = plusMinCtr <$> (char '+' <|> char '-')
  where
    plusMinCtr '+' = Sum
    plusMinCtr '-' = Minus

mulDivOp :: Parser String Operator
mulDivOp = mulDivCtr <$> (char '*' <|> char '/')
  where
    mulDivCtr '*' = Mul
    mulDivCtr '/' = Div

powOp = powCtr <$> char '^'
  where
    powCtr _ = Pow

cmpOp = eqCtr <$> eqRaw
  where
    eqRaw = string "=="
             <|> string "/="
             <|> string "<="
             <|> string "<"
             <|> string ">="
             <|> string ">"

    eqCtr "==" = Eq
    eqCtr "/=" = Neq
    eqCtr "<=" = Le
    eqCtr "<" = Lt
    eqCtr ">=" = Ge
    eqCtr ">" = Gt

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
      ident = (+1)

a = (BinOp Conj (BinOp Pow (Primary 1) (BinOp Sum (Primary 2) (Primary 3))) (Primary 4))

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

cmpPR (Right (_, ok)) c b = (ok == c) == b
cmpPR (Left _) _ _ = False

testParseEq = cmpPR (runParser cmpOp "==") Eq True
           && cmpPR (runParser cmpOp "<=") Le True
           && cmpPR (runParser cmpOp "<" ) Lt True
           && cmpPR (runParser cmpOp ">=") Ge True
           && cmpPR (runParser cmpOp ">" ) Gt True
test =
    let Right a1 = parseExpression "1+2+3"
        test1 = eval a1 == 6
        Right a2 = parseExpression "1+2*3"
        test2 = eval a2 == 7
        Right a3 = parseExpression "1*2+3"
        test3 = eval a3 == 5
        Right a4 = parseExpression "1^2^3"
        test4 = eval a4 == 1
        Right a5 = parseExpression "1&&0||1&&1"
        test5 = eval a5 == 1
        Right a6 = parseExpression "1||0&&0||1"
        test6 = eval a6 == 1
        Right a7 = parseExpression "10>4||10>=10"
        test7 = eval a7 == 1
    in and [test1, test2, test3, test4, test5, test6,test7]
