module Parser where

import           Control.Applicative                      ( (<|>) )
import           ParserCombinators
import           Ast
import Data.List ((\\))
import Data.Char (isLower, isUpper)
import Debug.Trace

programParser input =
  case (runParser (parseText <* eof) input) of
    Right (_, text) -> runParser (parseProgram <* eof) text
    Left x -> Left x

exprParser = runParser (parseExpr <* eof)

parseProgram =
  sepBy (betweenSpaces parseDecl) (betweenSpaces $ char ';')
    <* char ';'
    <* spaces

parseDecl = (DataDecl <$> parseData) <|> (BindDecl <$> parseBind) <|> parseType

keywords = ["if", "then", "else", "let", "in", "case", "of", "data", "T", "F"]
parseVarName = do
  i <- ident
  if i `elem` keywords then Parser $ \s -> Left "Indent is a keyword!" else pure i

parseType = do
  fn <- betweenSpaces (between (char ':') parseVarName)
  ts <- parsePrimeType'
  pure (TypeDecl fn ts)

parsePrimeType = do
  fn <- getVarType <$> parseVarName
  (do
      betweenSpaces (string "->")
      tn <- parsePrimeType
      pure (fn :-> tn)
    )
    <|> pure fn

getVarType "Int"  = TInt
getVarType "Bool" = TBool
getVarType x = TVar x

parsePrimeType' = do
  fn <- getVarType' <$> many (betweenSpaces parseVarName)
  (do
      betweenSpaces (string "->")
      tn <- parsePrimeType'
      pure (fn :-> tn)
   )
   <|> pure fn

getVarType' ["Int"]  = TInt
getVarType' ["Bool"] = TBool
getVarType' [x]
  | isLower (head x) = TVar x
getVarType' (x:xs)
  | isUpper (head x) = TData x (getVarType <$> xs)
getVarType' x = error $ "Bad data type: `" ++ show x ++ "'"

parseBind = do
  name <- parseVarName
  args <- many (betweenSpaces parsePat)
  betweenSpaces (string "=")
  spaces
  expr <- parseExpr
  pure (Bind name args expr)

-- Parse 'data'
-- data NAME = CTR ARG (| CTR ARG)*
parseData :: Parser String Data
parseData = do
  string "data"
  spaces
  dataName <- parseVarName
  spaces
  ts  <- fmap TypeVar <$> sepBy parseVarName spaces
  betweenSpaces (char '=')
  ctrs <- sepBy1 parseCtr (betweenSpaces $ char '|')
  pure (Data dataName ts ctrs)

parseCtr = do
  i    <- parseVarName
  -- args <- many (betweenSpaces (parseTypeNoArrow <|> char '(' *> parseTypeNoArrow <* char ')'))
  spaces
  args <- sepBy parseTypeNoArrow spaces
  pure $ Constr i args

parseTypeNoArrow = parseTypeNoArrow'

parseTypeNoArrow' = do
    char '('
    TData x _ <- getVarType <$> parseVarName
    spaces
    cs <- sepBy parseTypeNoArrow' spaces
    char ')'
    pure (TData x cs)
  <|>
   getVarType <$> parseVarName
  where
    getVarType "Int"  = TInt
    getVarType "Bool" = TBool
    getVarType x
      | isLower (head x) = TVar x
      | isUpper (head x) = TData x []


    getVarType' ["Int"]  = TInt
    getVarType' ["Bool"] = TBool
    getVarType' (x:xs)
      | isLower (head x) = TVar x
    getVarType' (x:xs)
      | isUpper (head x) = TData x (getVarType <$> xs)
    getVarType' x = error $ "Bad data type: `" ++ show x ++ "'"


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

parseAtom =
    parseIf
    <|> parseLet
    <|> parseCase
    <|> parseApp
    <|>  parseLit
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

parseCasePat =
  parsePatLit
    <|> parsePatCtr
    <|> parsePatVar
    <|> (char '(' *> parseCasePat <* char ')')

parsePat =
  parsePatLit
    <|> parsePatCtr1
    <|> parsePatVar
    <|> (char '(' *> parsePat' <* char ')')
parsePat' =
  parsePatLit
    <|> parsePatCtr
    <|> parsePatVar
    <|> (char '(' *> parsePat <* char ')')

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
  c  <- orChar ['A' .. 'Z']
  cs <- many (orChar identChars)
  pure (c : cs)

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
  where orOp = LO <$> (const Or <$> betweenSpaces (string "||"))

parseAnd = (binOp <$> parseCmp <*> andOp <*> parseAnd) <|> parseCmp
  where andOp = LO <$> (const And <$> betweenSpaces (string "&&"))

parseCmp = (binOp <$> parsePM <*> cmpOp <*> parsePM) <|> parsePM
 where
  cmpOp =
    CO
      <$> (   (const Eq <$> betweenSpaces (string "=="))
          <|> (const Neq <$> betweenSpaces (string "/="))
          <|> (const Le <$> betweenSpaces (string "<="))
          <|> (const Lt <$> betweenSpaces (string "<"))
          <|> (const Ge <$> betweenSpaces (string ">="))
          <|> (const Gt <$> betweenSpaces (string ">"))
          )

parsePM = do
  op   <- parseMD
  asts <- many $ do
    op' <- pmOp
    a   <- parseMD
    pure (\b -> BinOp op' b a)
  pure (foldl (\e f -> f e) op asts)
 where
  pmOp =
    AO
      <$> (   (const Sum <$> betweenSpaces (string "+"))
          <|> (const Sub <$> betweenSpaces (string ".-"))
          )

parseMD = do
  op   <- parsePow
  asts <- many $ do
    op' <- mdOp
    a   <- parsePow
    pure (\b -> BinOp op' b a)
  pure (foldl (\e f -> f e) op asts)
 where
  mdOp =
    AO
      <$> (   const Mul
          <$> betweenSpaces (string "*")
          <|> const Div
          <$> betweenSpaces (string "/")
          )

parsePow = (binOp <$> parseUnOp <*> powOp <*> parsePow) <|> parseUnOp
  where powOp = AO <$> (const Pow <$> betweenSpaces (string "^"))

-- Parse un ops
parseUnOp =
  (   UnOp
    <$> unOp
    <*> (parseVar <|> parseLit <|> (char '(' *> parseAtom <* char ')'))
    )
    <|> parseAtom
 where
  unOp =
    (const Minus <$> betweenSpaces (string "-"))
      <|> (const Neg <$> betweenSpaces (string "!"))

-- Parse literals
parseLit :: Parser String Expr
parseLit = Lit <$> parseLit'

parseLit' = (ILit <$> number) <|> (BLit <$> parseBool)

-- Parse boolean literal
parseBool :: Parser String Bool
parseBool =
  do
      char 'T'
      pure True
    <|> do
          char 'F'
          pure False


parseMultiLine :: Parser String Char
parseMultiLine = do
  string "{-"
  many (orChar (allchars  \\ "-") <|> notend)
  string "-}"
  pure ' '
  where notend = do
          c1 <- char '-'
          b <- checkNext '}'
          if b
          then Parser (const $ Left "Bad placed comments.")
          else pure c1


parseMultiLineNested :: Parser String Char
parseMultiLineNested = do
  string "{-"
  spaces
  many ((char '-' *> notchar '}')
     <|> parseMultiLineNested *> pure 'a'
     <|> (orChar $ allchars  \\ "-"))
  spaces
  b <- checkNexts "-}"
  if not b
  then failP "Comments not nested"
  else do
    string "-}"
    pure ' '

parseOneLine :: Parser String String
parseOneLine = do
  string "--"
  many (notchar '\n')
  newline <|> eof
  pure ""

anychar1 = do
    c1 <- char '{'
    b <- checkNext '-'
    if b
    then failP "Cannot parse next chars: bad placed comments."
    else pure c1

anychar2 = do
    c1 <- char '-'
    b1 <- checkNext '}'
    b2 <- checkNext '-'
    if b1 || b2
    then failP "Cannot parse next chars: bad placed comments."
    else pure c1

anychar3 = orChar $ allchars \\ "{-\n"

anychar' = anychar1 <|> anychar2 <|> anychar3

spacesEnd = parseOneLine
         <|> (newline <|> eof) *> pure ""

p1 = many anychar' <* spacesEnd
p2 ml = do
  b1 <- checkNexts "{-"
  if b1
  then do
    c <- some ml
    r <- many anychar'
    b <- checkNexts "-}"
    if b
    then failP "Cannot parse next chars: bad placed comments."
    else do
      rest <- eof *> pure "" <|> spacesEnd <|> p2 ml
      pure (c ++ r ++ rest)
  else do
    r <- many anychar'
    b <- checkNexts "-}"
    if b
    then failP "Cannot parse next chars: bad placed comments."
    else do
      rest <- eof *> pure "" <|> spacesEnd <|> p2 ml
      pure (r ++ rest)


parseText = do
  text <- p2 parseMultiLine
  rest <- eof *> pure "" <|> parseText
  pure (text ++ rest)


parseTextNested = do
  text <- p2 parseMultiLineNested
  rest <- eof *> pure "" <|> parseTextNested
  pure (text ++ rest)

test1'
  | Right (_, "") <- runParser parseOneLine "-- 123"
  , Right (_, "") <- runParser parseOneLine "-------"
  = True
  | otherwise
  = False

test1''
  | Right (_, "") <- runParser spacesEnd "-- 123"
  , Right (_, "") <- runParser spacesEnd "-------"
  = True
  | otherwise
  = False

test1
  | Right (_, "123 ") <- runParser p1 "123 -- 123"
  , Right (_, " 124 ") <- runParser p1 " 124 -----"
  , Right (_, " -124 ") <- runParser p1 " -124 -----"
  , Right (_, "") <- runParser p1 "-----"
  = True
  | otherwise = False

test2'
  | Right (_, ' ') <- pm "{- comment -}"
  , Right (_, ' ') <- pm "{- -123 -}"
  , Right (_, ' ') <- pm "{- comment-comment -}"
  , Right (_, ' ') <- pm "{- comment\ncomment -}"
  , Right (_, ' ') <- pm "{- -}"
  , Right (_, ' ') <- pm "{--}"
  = True
  | otherwise
  = False
  where pm = runParser parseMultiLine

test2''
  | Right (_, ' ') <- pm "{- comment -}"
  , Right (_, ' ') <- pm "{- -123 -}"
  , Right (_, ' ') <- pm "{- comment-comment -}"
  , Right (_, ' ') <- pm "{- comment\ncomment -}"
  , Right (_, ' ') <- pm "{- -}"
  , Right (_, ' ') <- pm "{--}"
  , Right (_, ' ') <- pm "{- {- -} -}"
  , Right (_, ' ') <- pm "{- {- {- {- -} -} -} -}"
  , Left _ <- pm "{- {- -}"
  , Left _ <- pm "{- {- -} -} -}"
  = True
  | otherwise
  = False
  where pm = runParser (parseMultiLineNested <* eof)

test2
  | Right (_, " ") <- pm "{- comment -}"
  , Right (_, " ") <- pm "{- -123 -}"
  , Right (_, " ") <- pm "{- comment-comment -}"
  , Right (_, " ") <- pm "{- comment\ncomment -}"
  , Right (_, " ") <- pm "{- -}"
  , Right (_, " ") <- pm "{--}"
  , Right (_, "  ") <- pm "{--}{--}"
  , Right (_, "   ") <- pm "{-comment1-}{-comment2-}{- -}"
  , Right (_, " ") <- pm "{- {- -}"
  , Left _        <- runParser (p2 parseMultiLine <* eof) "{- {- -} -}"
  , Right (_, " 123 ") <- pm "{- comment -}123{- comment -}-- comment"
  , Right (_, " 123  abc ") <- pm "{- comment -}123{- comment -} abc -- comment"
  , Right (_, "  123   abc   456   def ") <- pm "{--} 123 {--} abc {--} 456 {--} def --"
  , Right (_, "123\t ") <- pm "123\t -- comment"
  , Right (_, "\t123\t ") <- pm "\t123\t -- comment"
  , Right (_, "abcd  ") <- pm "abcd {- {- -}"
  = True
  | otherwise
  = False
  where pm = runParser (p2 parseMultiLine)

test2Nested
  | Right (_, " ") <- pm "{- comment -}"
  , Right (_, " ") <- pm "{- -123 -}"
  , Right (_, " ") <- pm "{- comment-comment -}"
  , Right (_, " ") <- pm "{- comment\ncomment -}"
  , Right (_, " ") <- pm "{- -}"
  , Right (_, " ") <- pm "{--}"
  , Right (_, "  ") <- pm "{--}{--}"
  , Right (_, "   ") <- pm "{-comment1-}{-comment2-}{- -}"
  , Left _ <- pm "{- {- -}"
  , Right (_, " ") <- pm "{- {- -} -}"
  , Right (_, " 123 ") <- pm "{- comment -}123{- comment -}-- comment"
  , Right (_, " 123  abc ") <- pm "{- comment -}123{- comment -} abc -- comment"
  , Right (_, "  123   abc   456   def ") <- pm "{--} 123 {--} abc {--} 456 {--} def --"
  , Right (_, "123\t ") <- pm "123\t -- comment"
  , Right (_, "\t123\t ") <- pm "\t123\t -- comment"
  , Left _ <- pm "abcd {- {- -}"
  , Right (_, "abcd  ") <- pm "abcd {- {- -} -}"
  = True
  | otherwise
  = False
  where pm = runParser (p2 parseMultiLineNested)

runAllTests = all id [test1, test1', test1'', test2', test2'', test2, test2Nested]
