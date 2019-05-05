module Automaton where


import AutomatonType
import Combinators
import ListParserCombinator
import Minimizer
import Determiner
import Completer
import EpsilonClosure

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List (groupBy, sort)
import Data.Maybe (isJust, fromJust)
import Control.Applicative ((<|>))

type Set = Set.Set
type Map = Map.Map


-- Checks if the automaton is nondeterministic (eps-transition or multiple transitions for a state and a symbol)
isNFA :: Automaton a b -> Bool
isNFA _ = True

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

    toStates = Set.fromList . fmap State

    toInitState (q:[]) = Just (State q)
    toInitState _ = Nothing

    toTermState = Set.fromList . fmap State

    toDelta [] = Map.empty
    toDelta ss = toDelta' ((\xs@((a1, a2, _):_) -> (a1, a2, (\(_,_,b) -> b) <$> xs)) <$> groupBy (\(a1, a2,_) (b1, b2,_) -> a1 == b1 && a2 == b2) (sort ss))

    toDelta' [] = Map.empty
    toDelta' ((src,symb,dists):ss) | symb == "\\epsilon" = Map.insert (State src,Nothing) (Set.fromList $ State <$> dists) $ toDelta' ss
                                   | otherwise = Map.insert (State src, Just symb) (Set.fromList $ State <$> dists) $ toDelta' ss

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
testND = "<a,b,c,d>,<1,2,3,4>,<1>,<3,4>,<(1,a,3), (1,a,2),(1,\\epsilon,4),(2,c,4), (2,c,1)>"
testC = "<a, b>, <1, 2>, <1>, <2>, <(1, a, 2), (1, b, 1), (2, a, 1), (2, b, 2)>"
testA = "<aa, bb, cc>, <stone, sttwo>, <stone>, <>, <>"

right (Right a) = a

testDFA = isDFA $ fromJust (right $ parseAutomaton testD)
testNFA = isNFA $ fromJust (right $ parseAutomaton testND)
testComplete = isComplete $ fromJust (right $ parseAutomaton testC)


autTxt = "<0,1>, <a,b,c,d,e,f,g>, <a>, <f,g>, <(a, 0, c), (a, 1, b), (b, 1, a), (b, 0, c), (c, 0, d), (c, 1, d), (d, 0, e), (d,1,f), (e,1,g), (e,0,f), (g,0,g), (g,1,f), (f, 0, f), (f, 1, f)>"
Right (Just t1) = parseAutomaton autTxt
autMinTxt = "<0, 1>, <ab, c, d, e, fg>, <ab>, <fg>, <(ab, 1, ab), (ab,0,c), (c,0,d), (c,1,d), (d,1,fg),(d,0,e),(e,0,fg),(e,1,fg), (fg,0,fg),(fg,1,fg)>"
Right (Just t2) = parseAutomaton autMinTxt
reachTxt = "<0,1>, <a,b,c,d,e,f,g,h>, <a>, <f,g>, <(a,0,h),(a,1,b),(b,1,a),(b,0,h),(h,0,c),(h,1,c),(c,0,e),(c,1,f),(e,0,f),(e,1,g),(d,0,e),(d,1,f),(g,0,g),(g,1,f),(f,1,f),(f,0,f)>"
Right (Just t3) = parseAutomaton reachTxt
Right (Just t4) = parseAutomaton "<1>, <a,b>, <a>, <b>, <(a,1,b), (b,1,a)>"
Right (Just t5) = parseAutomaton "<1, 0>, <a, b>, <a>, <b>, <(a,1,b)>"
Right (Just t6) = parseAutomaton "<aa>, <stone, sttwo>, <stone>, <sttwo>, <(stone, aa, sttwo)>"
Right (Just t7) = parseAutomaton testND
Right (Just t8) = parseAutomaton testD
enfaTxt = "<0, 1>, <a,b,c>, <a>, <b>, <(a, \\epsilon, b), (a, \\epsilon, a), (a, \\epsilon, c), (b, 0, c), (b, 1, a), (c, 1, b), (c, \\epsilon, a)>"
Right (Just t9) = parseAutomaton enfaTxt
enfaTxt2 = "<0>, <p, q, r, q1, r1, s, r2>, <p>, <q, r>, <(p, \\epsilon, q), (p, \\epsilon, r), (q, 0, q1), (r, 0, r1), (r, \\epsilon, s), (q1, 0, q), (r1, 0, r2), (r2, 0, r)>"
Right (Just t10) = parseAutomaton enfaTxt2
nfaTxt2 = "<0>, <p, q, r, q1, r1, r2>, <p>, <p, q, r>, <(p, 0, q1), (p, 0, r1), (q1, 0, q), (q, 0, q1), (r1, 0, r2), (r2, 0, r), (r, 0, r1)>"
Right (Just t11) = parseAutomaton nfaTxt2
txt = "<a,b>, <0,1,2,3,4,5>, <0>, <1,3,4,5>, <(0, a, 1), (1, a, 2), (2, a, 3), (3, a, 4), (4, a, 5), (5, a, 0), (0, a, 2), (1, a, 3), (2, a, 4), (3, a, 5), (4, a, 0), (5, a, 1), (0, b, 5), (5, b, 4), (4, b, 3), (3, b, 2), (2, b, 1), (1, b, 0) >"
Right (Just t12) = parseAutomaton txt
txt2 = "<a>, <b>, <b>, <b>, <(b, a, b), (b, \\epsilon, b)>"
Right (Just t13) = parseAutomaton txt2

tests = [t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13]
--runTests :: [Automaton q s] -> Bool
runTests = all doTest tests
  where
      doTest a = (isMinimal $ minimize a) && (isComplete $ complete a) && (isDFA $ determine a)
