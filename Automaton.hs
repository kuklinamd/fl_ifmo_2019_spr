module Automaton where


import AutomatonType
import Combinators
import ListParserCombinator
import Minimizer

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List (groupBy, sort)
import Data.Maybe (isJust, fromJust)
import Control.Applicative ((<|>))

type Set = Set.Set
type Map = Map.Map


-- Checks if the automaton is deterministic (only one transition for each state and each input symbol)
isDFA :: Automaton a b -> Bool
isDFA (Automaton _ _ _ _ delta) = all check $ Map.toList delta
  where
    check ((_, Nothing), _) = False
    check ((q, Just s), sts) = Set.size sts == 1

isNFA :: Automaton a b -> Bool
isNFA _ = True

-- Checks if the automaton is nondeterministic (eps-transition or multiple transitions for a state and a symbol)
isNFAUsefull :: Automaton a b -> Bool
isNFAUsefull a@(Automaton _ _ _ _ delta) = any check (Map.toList delta)
  where
    check ((_, Nothing), _) = True
    check ((q, Just s), sts) = Set.size sts > 1

-- Checks if the automaton is complete (there exists a transition for each state and each input symbol)
isComplete :: (Ord a, Ord b) => Automaton a b -> Bool
isComplete a@(Automaton sig sts _ _ delta) | not (isNFAUsefull a) = all check ((,) <$> Set.toList sts <*> Set.toList sig)
  where
    check (sig, st) = isJust $ Map.lookup (sig, Just st) delta
isComplete _ = False

-- Checks if the automaton is minimal (only for DFAs: the number of states is minimal)
isMinimal :: (Ord a, Ord b) => Automaton a b -> Bool
isMinimal a | not (isDFA a) = False
isMinimal a = null $ findEqual a

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
    toDelta ss = toDelta' ((\xs@((a1, a2, _):_) -> (a1, a2, (\(_,_,b) -> b) <$> xs)) <$> groupBy (\(a1, a2,_) (b1, b2,_) -> a1 == b1 && a2 == b2) (sort ss))

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

autTxt = "<0,1>, <a,b,c,d,e,f,g>, <a>, <f,g>, <(a, 0, c), (a, 1, b), (b, 1, a), (b, 0, c), (c, 0, d), (c, 1, d), (d, 0, e), (d,1,e), (e,1,g), (e,0,f), (g,0,g), (g,1,f), (f, 0, f), (f, 1, f)>"
Right (Just aut) = parseAutomaton autTxt

reachTxt = "<0,1>, <a,b,c,d,e,f,g,h>, <a>, <f,g>, <(a,0,h),(a,1,b),(b,1,a),(b,0,h),(h,0,c),(h,1,c),(c,0,e),(c,1,f),(e,0,f),(e,1,g),(d,0,e),(d,1,f),(g,0,g),(g,1,f),(f,1,f),(f,0,f)>"
Right (Just reachA) = parseAutomaton reachTxt

Right (Just a) = parseAutomaton "<1>, <a,b>, <a>, <b>, <(a,1,b), (b,1,a)>"

minTxt = "<a,b>, <0,1,2,3,4,5>, <0>, <1,3,4,5>, <(0, a, 1), (1, a, 2), (2, a, 3), (3, a, 4), (4, a, 5), (5, a, 0), (0, b, 5), (5, b, 4), (4, b, 3), (3, b, 2), (2, b, 1), (1, b, 0) >"
Right (Just minA) = parseAutomaton minTxt

nfaTxt = "<a, b>, <1, 2, 3>, <1>, <1,3>, <(1, a, 1), (2, b, 1), (1, b, 2), (2, b, 2), (2, a, 3), (3, a, 2), (3, b, 3)>"
Right (Just nfaA) = parseAutomaton nfaTxt

deltaTxt ="<(1, a, 1), (2, b, 1), (1, b, 2), (2, b, 2), (2, a, 3), (3, a, 2), (3, b, 3)>"
