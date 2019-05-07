module Parser where

import Control.Applicative ((<|>))
import ParserCombinators
import Ast

program = "data Data = A Int | B | C Bool Int;\nmain = let x = 10 in x ^ x;"

parseProgram = sepBy1 (betweenSpaces parseDecl) (char ';') <* char ';'

parseDecl = (DataDecl <$> parseData) <|> (BindDecl <$> parseBind)

parseBind = do
  name <- ident
  args <- many (betweenSpaces parsePat)
  betweenSpaces (string "=")
  expr <- parseExpr
  pure (Bind name args expr)

-- Parse 'data'
-- data NAME = CTR ARG*
parseData :: Parser String Data
parseData = do
  string "data"
  spaces
  dataName <- ident
  betweenSpaces (char '=')
  ctrs <- sepBy1 parseCtr (betweenSpaces $ char '|')
  pure (Data dataName ctrs)

parseCtr = do
  i <- ident
  args <- many (betweenSpaces ident)
  pure $ Constr i args

-- Parse 'expr'
--
-- Examples:
--
-- Var: x
--
-- Lit: 12
-- Lit: T
--
-- App: f x
--
-- If: if x == 0 then 0 else 1
--
-- Case: case x of { Pat1 -> 1; Pat2 -> 3;}
--
-- Let: let x = 10 in x + 12
--
-- UnOp: -1, !F
--
-- BinOp: 1+2, 1 * (1 - 20^(11 + 4)), 1 * x - y * f x
--
parseExpr :: Parser String Expr
parseExpr = parseBinOp

parseAtom = parseLit
          <|> parseIf
          <|> parseLet
          <|> parseCase
          <|> parseApp
          <|> (char '(' *> parseExpr <* char ')')

-- Parse application

parseArg = parseLit
         <|> parseVar
         <|> (char '(' *> parseExpr <* char ')')

parseApp = do
    fn <- parseVar <|> (char '(' *> parseExpr <* char ')')
    spaces
    args <- sepBy parseArg spaces
    pure $ foldl App fn args

-- Parse Case
-- case (expr) of {(| pat -> expr) +}
parseCase = do
   betweenSpaces (string "case")
   e <- char '(' *> parseExpr <* char ')'
   betweenSpaces (string "of")
   ps <- char '{' *> some (betweenSpaces parseCaseBody) <* char '}'
   pure (Case e ps)

parseCaseBody = do
    betweenSpaces (string "|")
    pat <- parsePat
    betweenSpaces (string "->")
    expr <- parseExpr
    pure (pat, expr)

parsePat = parsePatCtr <|> parsePat'

parsePat' = parsePatVar <|> parsePatLit <|> (char '(' *> parsePat <* char ')')

parsePatLit = PatLit <$> parseLit'

parsePatVar = PatVar <$> ident

parsePatCtr = do
  cname <- constr
  spaces
  ids <- sepBy parsePat' spaces
  pure (PatCtr cname ids)
      
constr = do
    c <- orChar ['A'..'Z']
    cs <- many (orChar identChars)
    pure (c:cs)

-- Parse Let
parseLet = do
    betweenSpaces (string "let")
    name <- ident
    betweenSpaces (string "=")
    expr <- parseExpr
    betweenSpaces (string "in")
    expr2 <- parseExpr
    pure (Let name expr expr2)

-- Parse if

parseIf = do
    betweenSpaces (string "if")
    cnd <- betweenSpaces parseExpr
    betweenSpaces (string "then")
    thn <- betweenSpaces parseExpr
    betweenSpaces (string "else")
    els <- betweenSpaces parseExpr
    pure (If cnd thn els)


-- Parse variable

parseVar = Var <$> ident

-- Parse arithm ops
parseBinOp = parseOr

binOp = flip BinOp

parseOr = (binOp <$> parseAnd <*> orOp <*> parseOr) <|> parseAnd
  where
    orOp = LO <$> (const Or <$> betweenSpaces (string "||"))

parseAnd = (binOp <$> parseCmp <*> andOp <*> parseAnd) <|> parseCmp
  where
    andOp = LO <$> (const And <$> betweenSpaces (string "&&"))

parseCmp = (binOp <$> parsePM <*> cmpOp <*> parsePM) <|> parsePM
  where
    cmpOp = CO <$> ((const Eq <$> betweenSpaces (string "=="))
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
    pmOp = AO <$> ((const Sum <$> betweenSpaces (string "+"))
              <|>  (const Sub <$> betweenSpaces (string ".-")))

parseMD = do
    op <- parsePow
    asts <- many $ do
        op' <- mdOp
        a   <- parsePow
        pure (\b -> BinOp op' b a)
    pure (foldl (\e f -> f e) op asts)
  where
   mdOp = AO <$> (const Mul <$> betweenSpaces (string "*")
             <|>  const Div <$> betweenSpaces (string "/"))

parsePow = (binOp <$> parseAtom <*> powOp <*> parsePow) <|> parseAtom
  where
    powOp = AO <$> (const Pow <$> betweenSpaces (string "^"))

-- Parse literals
parseLit :: Parser String Expr
parseLit = Lit <$> parseLit'

parseLit' = (ILit <$> number) <|> (BLit <$> parseBool)


-- Parse boolean literal
parseBool :: Parser String Bool
parseBool = do {char 'T'; pure True} <|> do {char 'F'; pure False}
