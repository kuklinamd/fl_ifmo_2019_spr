module Expression where

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
parseExpression input = snd <$> runParser exprP input

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

cmpPR (Right (_, ok)) c b = (ok == c) == b
cmpPR (Left _) _ _ = False

testParseEq = cmpPR (runParser cmpOp "==") Eq True
           && cmpPR (runParser cmpOp "<=") Le True
           && cmpPR (runParser cmpOp "<" ) Lt True
           && cmpPR (runParser cmpOp ">=") Ge True
           && cmpPR (runParser cmpOp ">" ) Gt True
