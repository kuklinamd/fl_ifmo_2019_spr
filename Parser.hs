module Parser where

import Control.Applicative ((<|>))
import ParserCombinators
import Ast

programParser = runParser (parseProgram <* eof)

parseProgram = sepBy (betweenSpaces parseDecl) (betweenSpaces $ char ';') <* char ';' <* spaces

parseDecl = (DataDecl <$> parseData)
        <|> (BindDecl <$> parseBind)
        <|> parseType

keywords = ["if", "then", "else", "let", "in", "case", "of", "data", "T", "F"]
parseVarName = do
    i <- ident
    if i `elem` keywords
    then Parser $ \s -> Left "Fail"
    else pure i

parseType = do
    fn <- betweenSpaces (between (char ':') parseVarName)
    ts <- parsePrimeType
    pure (TypeDecl fn ts)

--parsePrimeType = do
parsePrimeType = do
  fn <- getVarType <$> parseVarName
  ((do
     betweenSpaces (string "->")
     tn <- parsePrimeType
     pure (Arrow fn tn))
     <|>
   (do
    pure fn))

getVarType "Int" = TInt
getVarType "Bool" = TBool
getVarType x = TData x

parseBind = do
  name <- parseVarName
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
  dataName <- parseVarName
  betweenSpaces (char '=')
  ctrs <- sepBy1 parseCtr (betweenSpaces $ char '|')
  pure (Data dataName ctrs)

parseCtr = do
  i <- parseVarName
  args <- many (betweenSpaces parseVarName)
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
-- Case: case x of { | Pat1 -> 1 | Pat2 -> 3}
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
    pat <- parseCasePat
    betweenSpaces (string "->")
    expr <- parseExpr
    pure (pat, expr)

parseCasePat = parsePatLit <|> parsePatCtr <|> parsePatVar <|> (char '(' *> parseCasePat <* char ')')

parsePat = parsePatLit <|> parsePatCtr1 <|> parsePatVar <|> (char '(' *> parsePat' <* char ')')
parsePat' = parsePatLit <|> parsePatCtr <|> parsePatVar <|> (char '(' *> parsePat <* char ')')

parsePatLit = PatLit <$> parseLit'

parsePatVar = PatVar <$> parseVarName

parsePatCtr1 = do
  cname <- constr
  pure (PatCtr cname [])

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
    name <- parseVarName
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

parseVar = Var <$> parseVarName

-- Parse bin ops
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

parsePow = (binOp <$> parseUnOp <*> powOp <*> parsePow) <|> parseUnOp
  where
    powOp = AO <$> (const Pow <$> betweenSpaces (string "^"))

-- Parse un ops
parseUnOp = (UnOp <$> unOp <*> (parseLit <|> parseVar <|> (char '(' *> parseAtom <* char ')'))) <|> parseAtom
  where
    unOp = (const Minus <$> betweenSpaces (string "-")) <|> (const Neg <$> betweenSpaces (string "!"))

-- Parse literals
parseLit :: Parser String Expr
parseLit = Lit <$> parseLit'

parseLit' = (ILit <$> number) <|> (BLit <$> parseBool)

-- Parse boolean literal
parseBool :: Parser String Bool
parseBool = do {char 'T'; pure True} <|> do {char 'F'; pure False}
