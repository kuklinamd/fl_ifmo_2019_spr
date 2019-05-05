module Minimizer {-(isMinimal, minimize)-} where
    
import AutomatonType
import Determiner 

import Data.Maybe (fromJust)

import qualified Data.Set as Set hiding (unions)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq

import qualified Data.List as List

import Data.Function (on)

import Control.Applicative (liftA2)

type Seq = Seq.Seq

-- Checks if the automaton is minimal (only for DFAs: the number of states is minimal)
isMinimal :: (Ord a, Ord b) => Automaton a b -> Bool
isMinimal a | not (isDFA a) = False
isMinimal a = null $ findEqual' a

minimize :: (Ord q, Ord s) => Automaton s [q] -> Automaton s [q]
minimize a | not (isDFA a)  = minimize $ determine a
minimize a = toSimple (minimizeCommon a)
  where
    toSimple (Automaton sig sts init term dlt) = Automaton sig sts' init' term' dlt'
      where
        sts' = Set.map toSingleState sts
        init' = toSingleState init
        term' = Set.map toSingleState term
        dlt' = f dlt

-- type Delta q s = Map (q, Maybe s) (Set q)
f :: (Ord q, Ord s) => Delta (State (Set (State [q]))) s -> Delta (State [q]) s
f = Map.map (Set.map toSingleState) . Map.mapKeys (\(a, b) -> (toSingleState a, b))

toSingleState :: (Ord q) => State (Set (State [q])) -> State [q]
toSingleState (State s) = (State . concatMap state . Set.toList) s
toSingleState Bot = Bot

minimizeCommon :: (Ord q, Ord s) => Automaton s q -> Automaton s (Set (State q))
minimizeCommon a@(Automaton sig st init term dlt) = let
    -- List of sets, that contains equal states.
    eq = Set.toList $ equivClass $ Set.toList $ findEqual' a --findEqual a
    -- Set of not equals states.
    rest = restStates st eq
    lrest = Set.singleton <$> Set.toList rest
    newStates = eq ++ lrest

    -- Set of names of new states.
    newInit   = State $ newInitSet $ filter (Set.member init) eq
    newTerm   = Set.singleton (State term)
  in Automaton sig (Set.fromList $ State <$> newStates) newInit newTerm (newDelta dlt newStates newStates Map.empty)
  where
    restStates st eq = st Set.\\ unions (Set.fromList eq)

    newInitSet [] = Set.singleton init
    newInitSet (s:[]) = s
    newInitSet _ = error $ "Init state more than at one equals states."

    newDelta dlt st [] mp = mp
    newDelta dlt st (s:sts) mp = let
        trans = map (\k -> Map.filterWithKey (stateMatches k)  dlt) $ Set.toList s
        in newDelta dlt st sts $ foldr (\t mp -> f mp (Map.toList t)) mp trans
        where
          f mp [] = mp
          f mp (((_ , symb), s2):ms) = let
              ns = findNewState st (Set.elemAt 0 s2)
              mp' = Map.insert (State s, symb) (Set.singleton (State ns)) mp
           in f mp' ms

    findNewState [] s2 = error "Empty list of states!"
    findNewState (s:st) s2 | s2 `Set.member` s = s
    findNewState (s:st) s2 = findNewState st s2

-- Reverse Delta ->
-- Terminal States ->
-- Sigs ->
-- Init set ->
-- List of set of equal states
--findEqual' :: (Ord q, Ord s) => Delta q s -> Seq q -> Set s -> Set (q, q) -> [Set q]
findDiffering rdlt term sigma qs = findDiffering' (Set.fromList qs) qs
  where
    findDiffering' m [] = m
    findDiffering' marked ((q1,q2):qs) =
      uncurry findDiffering' $ Set.foldr f (marked, qs) sigma
      where

        -- Symbol -> (Marked, Queue) -> (Marked, Queue)
        f symb (marked,qs) = foldr p (marked, qs) $ filter (uncurry (<)) $ all_pairs rs ss
          where
            -- List of states to go to from q1 and q2.
            rs = maybe [] Set.toList $ Map.lookup (q1, Just symb) rdlt
            ss = maybe [] Set.toList $ Map.lookup (q2, Just symb) rdlt
        -- State -> State -> (Marked, Queue) -> (Marked, Queue)
        p pr (mrkd, que) =
          if pr `Set.member` mrkd
            then (mrkd, que)
            else (Set.insert pr mrkd, pr:que)

findEqual' :: (Ord s, Ord q) => Automaton s q -> Set.Set (State q, State q)
findEqual' a | not (isDFA a) = error "Not a DFA!"
findEqual' a = let rdlt = revDelta (states a) (delta a)
                   term = termState a
                   sig  = sigma a
                   qs = initTable' (states a) term
                   st = Set.toList $ states a
                in (Set.fromList $ filter (uncurry (<)) $ all_pairs st st)
                    Set.\\ findDiffering rdlt term sig qs

setGroupBy :: Ord a => (a -> a -> Bool) -> Set a -> Set (Set a)
setGroupBy p = uncurry put . Set.foldr f ([], Set.empty)
  where f el ([], ac) = ([el], ac)
        f el ((x:xs), ac) | p el x = (el:x:xs, ac)
                          | otherwise = ([el], put (x:xs) ac)
        put ls ac = Set.insert (Set.fromList ls) ac

equivClass :: Ord a => [(a, a)] -> Set (Set a)
equivClass = equivClass' Set.empty
  where
    equivClass' eqc [] = eqc
    equivClass' eqc ((a, b):rs) = let
      (ain, other) = Set.partition (Set.member a) eqc
      (bin, other') = Set.partition (Set.member b) other
      neqc = Set.insert a $ Set.insert b $ Set.union (unions ain) (unions bin)
      in equivClass' (Set.insert neqc other') rs


initTable' :: (Ord q) => Set q -> Set q -> [(q, q)]
initTable' qs term =
  let ql = Set.toList qs
      allPairs = (,) <$> ql <*> ql
      unorderedPairs = filter (uncurry (<)) allPairs
   in filter (uncurry ((/=) `on` (\q -> Set.member q term))) unorderedPairs

all_pairs :: Eq a => [a] -> [a] -> [(a, a)]
all_pairs a b = filter (uncurry (/=)) $ (,) <$> a <*> b

reachable :: (Ord q, Ord s) => q -> Delta q s -> Set q
reachable start dlt = Map.foldr (Set.union . go) (Set.singleton start) walked
  where
    (walked, unwalked) = Map.partitionWithKey (stateMatches start) dlt
    go = unions . Set.map (\ntost -> reachable ntost unwalked)

unions :: Ord a => Set.Set (Set.Set a) -> Set.Set a
unions = Set.foldr Set.union Set.empty

-- Find all transitions from `st` state with each symbol.
stateMatches st (k, _) _ = st == k

-- For each state we look where it leads by each symbol
-- and insert these values to our new delta.
revDelta :: (Ord q, Ord s) => Set q -> Delta q s -> Delta q s
revDelta sts dlt = Set.foldr f Map.empty sts
  where
    -- Find all states from q and add them at rev delta.
    --f :: q -> Delta q s -> Delta q s
    f st revDlt = Map.foldrWithKey h revDlt (m st)

    -- Find states and symbols from q.
    m q = Map.filterWithKey (stateMatches q) dlt

    -- As FSM must be deterministic, there's only one new state
    h (stt, symb) newsts | [newst] <- Set.toList newsts =
        let key = (newst, symb)
            initOrInsert = maybe (Set.singleton stt) (Set.insert stt)
        in Map.alter (Just . initOrInsert) key
    h _ _ = error "Bad automaton input"
