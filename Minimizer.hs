module Minimizer (findEqual) where
    
import AutomatonType

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq

import qualified Data.List as List

import Debug.Trace

type Seq = Seq.Seq

findEqual :: (Ord q, Ord s) => Automaton s q -> [Set q]
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

       func Nothing = Just True
       func _  = Just True
    markTable' tabl (_:rs) = markTable' tabl rs

    mapDiff = Map.differenceWith (\a b -> if a == False && b == True then Just b else Nothing)

    -- In t only True
    updateQue t q = foldr (\a b -> b Seq.|> a) q (fst <$> Map.toList t)


initTable :: (Ord q) => Set q -> Set q -> (Map (q, q) Bool, Seq (q,q))
initTable sts term = (Map.fromList lst, Seq.fromList {-(fst <$> lst)-} $ filterUniqSet sts term)
  where
    stlist = Set.toList sts

    toMapList [] = []
    toMapList (q@(q1,q2):qs) | Set.member q1 term = (q, True) : toMapList qs
    toMapList (q@(q1,q2):qs) | Set.member q2 term = (q, True) : toMapList qs
    toMapList (q@(q1,q2):qs) = (q, False) : toMapList qs

    lst = toMapList $ filterUniqSet sts sts

    filterUniqSet l1 l2 = filter (\(f,s) -> f /= s && not (f `Set.member` term && s `Set.member` term)) ((,) <$> Set.toList l1 <*> Set.toList l2)

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
