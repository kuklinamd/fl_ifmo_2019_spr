module Automaton where

import Combinators
import ListParserCombinator

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List (groupBy)
import Data.Maybe (isJust, fromJust)
import Control.Applicative ((<|>))

type Set = Set.Set
type Map = Map.Map

data Automaton s q = Automaton { sigma     :: Set s
                               , states    :: Set q
                               , initState :: q
                               , termState :: Set q
                               , delta     :: Map (q, Maybe s) (Set q)
                               } deriving Show


-- Checks if the automaton is deterministic (only one transition for each state and each input symbol)
isDFA :: Automaton a b -> Bool
isDFA (Automaton _ _ _ _ delta) = all check $ Map.toList delta
  where
    check ((_, Nothing), _) = False
    check ((q, Just s), sts) = Set.size sts == 1

-- Checks if the automaton is nondeterministic (eps-transition or multiple transitions for a state and a symbol)
isNFA :: Automaton a b -> Bool
isNFA a@(Automaton _ _ _ _ delta) = isDFA a || any check (Map.toList delta)
  where
    check ((_, Nothing), _) = True
    check ((q, Just s), sts) = Set.size sts > 1

-- Checks if the automaton is complete (there exists a transition for each state and each input symbol)
isComplete :: (Ord a, Ord b) => Automaton a b -> Bool
isComplete a@(Automaton sig sts _ _ delta) | not (isNFA a) = all check ((,) <$> Set.toList sts <*> Set.toList sig)
  where
    check (sig, st) = isJust $ Map.lookup (sig, Just st) delta
isComplete _ = False

-- Checks if the automaton is minimal (only for DFAs: the number of states is minimal)
isMinimal :: Automaton a b -> Bool
isMinimal a | not (isDFA a) = False
isMinimal (Automaton sig sts init term delta) = error "`isMinimal` is not implemented"

-- Top level function: parses input string, checks that it is an automaton, and then returns it.
-- Should return Nothing, if there is a syntax error or the automaton is not a correct automaton.
-- This includes:
-- * The set of states is empty
-- * The init state is not a state
-- * Any of the terminal states is not a state
-- * Delta function is defined on not-a-state or not-a-symbol-from-sigma
parseAutomaton :: String -> Either ParseError (Maybe (Automaton String String))
parseAutomaton s = checkAutomation <$> snd <$> (runParser parseLists s)
  where
    fst' :: Maybe ([a1], a2) -> Maybe a2
    fst' (Just ([], auto)) = Just auto
    fst' _                 = Nothing

    checkAutomation (_, _, Nothing, _, _) = Nothing
    checkAutomation (sgs, sts, Just init, terms, delta) = 
      let emptyStates = not . null $ sts
          initIsState = init `Set.member` sts
          termsAreStates = all (flip Set.member sts) terms
          deltaEntryCorrect ((q,s), q2) = q `Set.member` sts && not (null $ q2 `Set.intersection` sts)
                                         && if isJust s then (fromJust s) `Set.member` sgs else True
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
    toDelta ss = toDelta' ((\xs@((a1, a2, _):_) -> (a1, a2, (\(_,_,b) -> b) <$> xs)) <$> groupBy (\(a1, a2,_) (b1, b2,_) -> a1 == b1 && a2 == b2) ss)

    toDelta' [] = Map.empty
    toDelta' ((src,symb,dists):ss) | symb == "\\epsilon" = Map.insert (src,Nothing) (Set.fromList dists) $ toDelta' ss
                                   | otherwise = Map.insert (src, Just symb) (Set.fromList dists) $ toDelta' ss

    parseLists = betweenSpaces $ do
      symbList <- parseSymbolList 1
      betweenSpaces $ char ','
      stateList <- parseStateList
      betweenSpaces $ char ','
      startList <- parseStartList
      betweenSpaces $ char ','
      termList  <- parseTerminalList
      betweenSpaces $ char ','
      deltList  <- parseDeltaList
      eof
      return (toSigma symbList,
              toStates stateList,
              toInitState startList,
              toTermState termList,
              toDelta deltList)

    parseSymbolList = parseList (betweenSpaces (some symbol)) delim lbr rbr

    parseStateList = parseSymbolList 1
    parseStartList = parseSymbolList 1
    parseTerminalList = parseSymbolList 0

    parseDeltaList = parseList parseTriple delim lbr rbr 0

    parseTriple = char '(' *> do {
      s1 <- betweenSpaces (some symbol);
      betweenSpaces $ char ',';
      symb <- betweenSpaces (string "\\epsilon" <|> some symbol);
      betweenSpaces $ char ',';
      s2 <- betweenSpaces (some symbol);
      pure (s1, symb, s2)} <* char ')'


    symbol = betweenSpaces $ orChar $ ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0'..'9']
    delim  = betweenSpaces $ char ','
    lbr    = betweenSpaces $ char '<'
    rbr    = betweenSpaces $ char '>'

testD = "<a,b,c,d>,<1,2,3,4>,<1>,<3,4>,<(1,a,3),(2,c,4)>"
testND = "<a,b,c,d>,<1,2,3,4>,<1>,<3,4>,<(1,a,3),(1,\\epsilon,4),(2,c,4)>"
testC = "<a, b>, <1, 2>, <1>, <2>, <(1, a, 2), (1, b, 1), (2, a, 1), (2, b, 2)>"
testA = "<aa, bb, cc>, <stone, sttwo>, <stone>, <>, <>"

right (Right a) = a

testDFA = isDFA $ fromJust (right $ parseAutomaton testD)
testNFA = isNFA $ fromJust (right $ parseAutomaton testND)
testComplete = isComplete $ fromJust (right $ parseAutomaton testC)
