module ListParserCombinator (parseList) where

import Control.Applicative (Alternative, empty, (<|>))
import Combinators

import Data.Maybe (isJust, isNothing)

parseList :: Parser tok elem  -> -- Parser for the element.
             Parser tok delim -> -- Parser for the delimeter.
             Parser tok lbr   -> -- Parser for the left brace.
             Parser tok rbr   -> -- Parser for the right brace.
             Int              -> -- Minimum number of elements in the list.
             Parser tok [elem]
parseList elem delim lbr rbr num = Parser $ \s -> do
    let Parser p = lbr *> sepBy elem delim <* rbr
    (s', elems) <- p s
    if length elems < num
    then empty
    else pure (s', elems)


testSimple =
     isJust (runParser parseAs "(a)")
  && isJust (runParser parseAs "(a,a,a)")
  && isNothing (runParser parseAs "(a,)")
  && isNothing (runParser parseAs "(b,)")
  && isNothing (runParser parseAs "(a;)")
  where
    lbr = char '('
    rbr = char ')'
    elems = char 'a'
    delim = char ','
    parseAs = parseList elems delim lbr rbr 0

testSimpleMin =
     isNothing (runParser parseAs "(a)")
  && isJust (runParser parseAs "(a,a,a)")
  && isNothing (runParser parseAs "(a,)")
  && isNothing (runParser parseAs "(b,)")
  && isNothing (runParser parseAs "(a;)")
  where
    lbr = char '('
    rbr = char ')'
    elems = char 'a'
    delim = char ','
    parseAs = parseList elems delim lbr rbr 2


testMore =
     isJust (runParser parseAs "((a))")
  && isJust (runParser parseAs "((a@@a@@a))")
  && isNothing (runParser parseAs "((a@@))")
  && isNothing (runParser parseAs "((b@@))")
  && isNothing (runParser parseAs "((a;))")
  where
    lbr = string "(("
    rbr = string "))"
    elems = char 'a'
    delim = string "@@"
    parseAs = parseList elems delim lbr rbr 0
