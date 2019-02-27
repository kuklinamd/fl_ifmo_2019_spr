module ListParserCombinator where

import Control.Applicative (Alternative, empty, (<|>))
import Combinators

import Data.Maybe (isJust, isNothing)

parseList :: Parser tok elem  -> -- Parser for the element.
             Parser tok delim -> -- Parser for the delimeter.
             Parser tok lbr   -> -- Parser for the left brace.
             Parser tok rbr   -> -- Parser for the right brace.
             Integer          -> -- Minimum number of elements in the list.
             Parser tok [elem]
parseList elem delim lbr rbr num = lbr *> sepBy elem delim <* rbr
-- TODO: last parameter


test =
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
