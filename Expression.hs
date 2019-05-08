module Expression where

import Debug.Trace
import Text.Printf
import ParserCombinators
import Control.Applicative ((<|>), many)

data Operator = Pow
              | Mul
              | Div
              | Sum
              | Sub -- substruction
              | Eq
              | Neq
              | Le
              | Lt
              | Ge
              | Gt
              | Conj
              | Disj
      deriving Eq

data UnOperator = Neg  -- negation
                | Minus -- unary minus
      deriving Eq

-- Simplest abstract syntax tree for expressions: only binops are allowed
data EAst a = BinOp Operator (EAst a) (EAst a)
            | UnOp UnOperator (EAst a)
            | Primary a
            | Var String
      deriving Eq

primary = Primary <$> number
var = Var <$> ident

-- Change the signature if necessary
-- Constructs AST for the input expression
parseExpression :: String -> Either ParseError (EAst Integer)
parseExpression input = snd <$> runParser parseBinOp input

parseAtom = var <|> primary <|> char '(' *> parseBinOp <* char ')'

parseBinOp = parseOr

binOp = flip BinOp

parseOr = (binOp <$> parseAnd <*> orOp <*> parseOr) <|> parseAnd
  where
    orOp = (const Disj  <$> betweenSpaces (string "||"))

parseAnd = (binOp <$> parseCmp <*> andOp <*> parseAnd) <|> parseCmp
  where
    andOp = (const Conj <$> betweenSpaces (string "&&"))

parseCmp = (binOp <$> parsePM <*> cmpOp <*> parsePM) <|> parsePM
  where
    cmpOp = ((const Eq <$> betweenSpaces (string "=="))
               <|> (const Neq <$> betweenSpaces (string "/="))
               <|> (const Le <$> betweenSpaces (string "<="))
               <|> (const Lt <$> betweenSpaces (string "<"))
               <|> (const Ge <$> betweenSpaces (string ">="))
               <|> (const Gt <$> betweenSpaces (string ">")))

parsePM = do
    op <- parseMD
    asts <- many $ do
        op' <- pmOp
        a   <- parseMD
        pure (\b -> BinOp op' b a)
    pure (foldl (\e f -> f e) op asts)
  where
    pmOp = ((const Sum <$> betweenSpaces (string "+"))
              <|>  (const Sub <$> betweenSpaces (string ".-")))

parseMD = do
    op <- parsePow
    asts <- many $ do
        op' <- mdOp
        a   <- parsePow
        pure (\b -> BinOp op' b a)
    pure (foldl (\e f -> f e) op asts)
  where
   mdOp = (const Mul <$> betweenSpaces (string "*")
             <|>  const Div <$> betweenSpaces (string "/"))

parsePow = (binOp <$> parseUnOp <*> powOp <*> parsePow) <|> parseUnOp
  where
    powOp = (const Pow <$> betweenSpaces (string "^"))

-- Parse un ops
parseUnOp = (UnOp <$> unOp <*> (primary <|> var <|> (char '(' *> parseAtom <* char ')'))) <|> parseAtom
  where
    unOp = (const Minus <$> betweenSpaces (string "-")) <|> (const Neg <$> betweenSpaces (string "!"))

instance Show Operator where
  show Pow   = "^"
  show Mul   = "*"
  show Div   = "/"
  show Sum   = "+"
  show Sub   = ".-"
  show Eq    = "=="
  show Neq   = "/="
  show Le    = "<="
  show Lt    = "<"
  show Ge    = ">="
  show Gt    = ">"
  show Conj  = "&&"
  show Disj  = "||"

instance Show UnOperator where
  show Minus = "-"
  show Neg   = "!"

instance Show a => Show (EAst a) where
  show = show' 0
    where
      show' n t =
        (if n > 0 then printf "%s|_%s" (concat (replicate (n - 1) "| ")) else id)
        (case t of
                  BinOp op l r -> printf "%s\n%s\n%s" (show op) (show' (ident n) l) (show' (ident n) r)
                  UnOp op x -> printf "%s\n%s" (show op) (show' (ident n) x)
                  Primary x -> show x
                  Var x -> x)
      ident = succ

