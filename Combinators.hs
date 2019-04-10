{-# LANGUAGE ScopedTypeVariables #-}
module Combinators where
    
import ParserCombinators
import Control.Applicative ((<|>), Alternative, empty)
import Data.List (null)

-- Parsing result is either an error message or some payload and a suffix of the input which is yet to be parsed
--newtype Parser str ok err = Parser { runParser :: str -> Either err (str, ok) }

runParserUntilEof p inp = snd <$> (runParser (p <* eof) inp)

data Assoc = LAssoc -- left associativity
           | RAssoc -- right associativity
           | NAssoc -- not associative
  deriving Eq

-- General parser combinator for expressions
-- Binary operators are listed in the order of precedence (from lower to higher)
-- Binary operators on the same level of precedence have the same associativity
-- Binary operator is specified with a parser for the operator itself and a semantic function to apply to the operands
expression :: [(Assoc, [(Parser str b, a -> a -> a)])] ->
              Parser str a ->
              Parser str a
expression [] primary = primary
expression ((a, ops):as) primary | Just fn <- lookup a assocMap =
  let nextOp = expression as primary
      opPrs = buildOpPrs ops
  in fn opPrs nextOp

buildOpPrs :: [(Parser str b, a -> a -> a)] -> Parser str (a -> a -> a)
buildOpPrs [] = empty
buildOpPrs [(p, f)] = const f <$> p
buildOpPrs ((p, f):ps) = const f <$> p <|> buildOpPrs ps

--assocMap :: [(Assoc, (a -> b -> a -> a) -> Parser str b -> Parser str a -> Parser str a)]
assocMap = [(LAssoc, lassoc), (RAssoc, rassoc), (NAssoc, nassoc)]

rassoc :: Parser str (a -> a -> a)  -> Parser str a -> Parser str a
rassoc op nextOpP = let opP = flip id <$> nextOpP <*> op <*> opP <|> nextOpP in opP

nassoc :: Parser str (a -> a -> a) -> Parser str a -> Parser str a
nassoc op nextOpP = flip id <$> nextOpP <*> op <*> nextOpP <|> nextOpP

lassoc :: Parser str (a -> a -> a) -> Parser str a -> Parser str a
lassoc opP nextOpP = mainLassoc
  where
    mainLassoc = flip id <$> nextOpP <*> subOp <|> nextOpP

    subOp =
      (do
        op <- opP
        ex <- nextOpP
        f  <- subOp
        pure (flip id (f ex) op))
      <|>
      (do
      op <- opP
      ex <- nextOpP
      pure (flip id ex op))
