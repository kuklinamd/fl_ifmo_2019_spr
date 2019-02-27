module Automaton where

import Combinators;
import ListParserCombinator;

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Control.Monad (join)

type Set = Set.Set
type Map = Map.Map

data Automaton s q = Automaton { sigma     :: Set s
                               , states    :: Set q
                               , initState :: q
                               , termState :: Set q
                               , delta     :: Map (q, s) (Maybe q)
                               }
  deriving Show

-- Top level function: parses input string, checks that it is an automaton, and then returns it.
-- Should return Nothing, if there is a syntax error or the automaton is not a correct automaton.
-- This includes:
-- * The set of states is empty
-- * The init state is not a state
-- * Any of the terminal states is not a state
-- * Delta function is defined on not-a-state or not-a-symbol-from-sigma
-- Pick appropriate types for s and q
parseAutomaton :: String -> Maybe (Automaton Char Integer)
parseAutomaton s = join $ checkAutomation <$> fst' (runParser parseLists s)
  where
    fst' :: Maybe ([a1], a2) -> Maybe a2
    fst' (Just ([], auto)) = Just auto
    fst' _                 = Nothing

    checkAutomation (_, _, Nothing, _, _) = Nothing
    checkAutomation (sgs, sts, Just init, terms, delta) = 
      let emptyStates = not . null $ sts
          initIsState = init `Set.member` sts
          termsAreStates = all (flip Set.member sts) terms
          deltaEntryCorrect((q,s), Just q2) = q `Set.member` sts && q2 `Set.member` sts && s `Set.member` sgs
          deltaCorrect = all deltaEntryCorrect $  Map.toList delta
      in
      if emptyStates && initIsState && termsAreStates && deltaCorrect
      then 
      Just $ Automaton sgs sts init terms delta
      else Nothing

    toSigma  = Set.fromList

    toStates = Set.fromList

    toInitState (q:[]) = Just q
    toInitState _ = Nothing

    toTermState = Set.fromList

    toDelta [] = Map.empty
    toDelta ((src,symb,dist):ss) = Map.insert (src,symb) (Just dist) $ toDelta ss

    parseLists = do
      symbList <- parseSymbolList
      char ','
      stateList <- parseStateList
      char ','
      startList <- parseStartList
      char ','
      termList  <- parseTerminalList
      char ','
      deltList  <- parseDeltaList
      return (toSigma symbList,
              toStates stateList,
              toInitState startList,
              toTermState termList,
              toDelta deltList)

    parseSymbolList = parseList symbol delim lbr rbr 0

    parseNumList n = parseList number delim lbr rbr n

    parseStateList = parseNumList 1
    parseStartList = parseNumList 1
    parseTerminalList = parseNumList 1

    parseDeltaList = parseList parseTriple delim lbr rbr 0

    parseTriple = char '(' *> do {
      s1 <- number;
      char ',';
      symb <- symbol;
      char ',';
      s2 <- number;
      pure (s1, symb, s2)} <* char ')'


    symbol = orChar ['a' .. 'z']
    delim = char ','
    lbr = char '<'
    rbr = char '>'

testAutomata = "<a,b,c,d>,<1,2,3,4>,<1>,<3,4>,<(1,a,3),(2,b,4)>"
