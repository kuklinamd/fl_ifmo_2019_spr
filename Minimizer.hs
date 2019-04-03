module Minimizer (isMinimal, minimize) where
    
import AutomatonType
import Determiner (isDFA)

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq

import qualified Data.List as List

import Control.Applicative (liftA2)

type Seq = Seq.Seq

-- Checks if the automaton is minimal (only for DFAs: the number of states is minimal)
isMinimal :: (Ord a, Ord b) => Automaton a b -> Bool
isMinimal a | not (isDFA a) = False
isMinimal a = null $ findEqual a

minimize :: (Ord q, Ord s) => Automaton s [q] -> Automaton s [q]
minimize a | isMinimal a = a
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
    eq = findEqual a
    -- Set of not equals states.
    rest = restStates st eq
    lrest = Set.singleton <$> Set.toList rest
    newStates = eq ++ lrest

    -- Set of names of new states.
    newInit   = State $ newInitSet $ filter (Set.member init) eq
    newTerm   = Set.singleton (State term)
  in Automaton sig (Set.fromList $ State <$> newStates) newInit newTerm (newDelta dlt newStates newStates Map.empty)
  where
    restStates st [] = st
    restStates st (e:eq) = restStates (st Set.\\ e) eq

    newInitSet [] = Set.singleton init
    newInitSet (s:[]) = s
    newInitSet _ = error $ "Init state more than at one equals states."

    newDelta dlt st [] mp = mp
    newDelta dlt st (s:sts) mp = let
        trans = map (\k -> Map.filterWithKey (findPartKey k)  dlt) $ Set.toList s
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

findEqual :: (Ord q, Ord s) => Automaton s q -> [Set (State q)]
findEqual a = toSet (fst <$> Map.toList (Map.filter (\a -> not a) $ go tabl que))
  where
    toSet l = toSet' ((\(a1, a2) -> Set.fromList [a1, a2]) <$> l)

    toSet' [] = []
    toSet' b@(s:ss) = let a = uniteSets s ss in if (List.sort a) == (List.sort b) then a else toSet' a

    uniteSets s [] = [s]
    uniteSets s (s':ss) | not (null $ Set.intersection s s') = uniteSets (Set.union s s') ss
    uniteSets s (s':ss) = s' : uniteSets s ss
    
    revD = revDelta (states a) (delta a)

    (tabl, que) = initTable (states a) (termState a)

    go tabl que | Just (q1, q2) <- que Seq.!? 0 = let
        -- Maps with symbols and transitions.
        -- We need transitions: [q]
        rs = (\((_, Just a), s) -> (a, Set.toList s)) <$> getNextStates q1
        ss = (\((_, Just a), s) -> (a, Set.toList s)) <$> getNextStates q2

        p ((s,ls):rs) ss | Just ls' <- lookup s ss = (s, uniq ls ls') : p rs ss
        p [] ss = []
        p (_:rs) ss = p rs ss

        prs = p rs ss

        newTabl = markTable tabl prs
        newQue = updateQue (mapDiff tabl newTabl) (Seq.drop 1 que)

        in go newTabl newQue
    go tabl _ = tabl

    uniq l1 l2 = filter (\p -> fst p /= snd p) ((,) <$> l1 <*> l2)

    getNextStates q = Map.toList (Map.filterWithKey (findPartKey q) revD)

     --where

    markTable tabl [] = tabl
    markTable tabl ((symb, sts):ss) = markTable (markTable' tabl sts) ss

    markTable' tabl [] = tabl
    markTable' tabl ((r,s):rss) | r /= s = markTable' (Map.alter func keyR (Map.alter func key tabl)) rss
     where
       key  = (r, s)
       keyR = (s, r)

       func _ = Just True
    markTable' tabl (_:rs) = markTable' tabl rs

    mapDiff = Map.differenceWith (\a b -> if a == False && b == True then Just b else Nothing)

    -- In t only True
    updateQue t q = foldr (\a b -> b Seq.|> a) q (fst <$> Map.toList t)


initTable :: (Ord q) => Set q -> Set q -> (Map (q, q) Bool, Seq (q,q))
initTable sts term = (,) (Map.fromList tabl) (Seq.fromList queue)
  where
    tabl = markDiff term sts
    queue = fst <$> filter snd tabl

    xor True True = False
    xor False False = False
    xor _ _ = True
    isTerm term q1 q2 = let b1 = Set.member q1 term
                            b2 = Set.member q2 term
                        in xor b1 b2
    markDiff term s1 = (\q@(q1, q2) -> if isTerm term q1 q2 then (q, True) else (q, False)) <$> allStates s1
    allStates s1 = filter (uncurry (/=)) $ allWithAll s1 s1
    allWithAll s1 s2 = liftA2 (,) (Set.toList s1) (Set.toList s2)

reachable :: (Ord q, Ord s) => q -> Delta q s -> Set q
reachable _ dlt | Map.null dlt = Set.empty
reachable start dlt = Set.insert start $ Set.unions (go <$> allTrans)
  where
    allTrans = Map.toList $ Map.filterWithKey (findPartKey start) dlt
    newDlt = deleteKeys dlt $ fst <$> allTrans

    go (_, tost) | Set.null tost = Set.empty
    go (_, tost) = reachable (Set.elemAt 0 tost) newDlt

    deleteKeys mp [] = mp
    deleteKeys mp (k:ks) = deleteKeys (Map.delete k mp) ks

-- Find all transitions from `st` state with each symbol.
findPartKey st (k, _) _ | st == k = True
findPartKey _ _ _ = False

-- For each state we look where it leads by each symbol
-- and insert these values to our new delta.
revDelta :: (Ord q, Ord s) => Set q -> Delta q s -> Delta q s
revDelta sts dlt = Set.foldr f Map.empty sts
  where
      -- Find all states from q and add them at rev delta.
      --f :: q -> Delta q s -> Delta q s
      f st revDlt = g
        where
            -- Find states and symbols from q.
            m = Map.filterWithKey (findPartKey st) dlt
            g = foldr h revDlt (Map.toList m)

            -- As FSM must be deterministic, there's only one new state
            -- and no epsilon (Nothing) symbols.
            h ((stt, Just symb), newsts) mp | Set.size newsts == 1 = Map.alter func key mp
              where
                newst = Set.elemAt 0 newsts
                key = (newst, Just symb)
                func (Just a) = Just (Set.insert stt a)
                func Nothing = Just (Set.singleton stt)
            h _ _ = error "Bad automaton input"
