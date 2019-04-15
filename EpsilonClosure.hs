module EpsilonClosure (epsEliminator, eclose) where

import AutomatonType

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Data.Maybe (isNothing, isJust, fromJust)

import Debug.Trace

ecloses dlt sts = map ((,) <*> (eclose dlt Set.empty)) $ Set.toList sts

epsEliminator :: (Ord q, Ord s) => Automaton s q -> Automaton s q
--epsEliminator a | Just () <- sequence_ (snd <$> Map.keys (delta a)) = a
epsEliminator (Automaton sigs sts init term dlt) = Automaton sigs newSts init newTerm newDelta
  where
    eCl = map ((,) <*> (eclose dlt Set.empty)) $ Set.toList sts
    eCl' = filter (\(_,s) -> Set.size s > 1) eCl

    newTerm = Set.fromList $ fmap fst $ filter (not . Set.null . snd) ((\(s, es) -> (,) s $ Set.intersection es term) <$> eCl)
    newDelta = deleteEpsArcs $ addTr pairs $ dlt
    newSts = Set.fromList ((fst . fst) <$> Map.toList newDelta)

    deleteEpsArcs = Map.filterWithKey (\k a -> isJust . snd $ k)

    pairs = let s = Set.toList sts in (,) <$> s <*> s

    addTr [] d = d
    -- f -- from state
    -- t -- to state
    --
    -- If there's way from f to t using eclose, add an arc to delta.
    -- ss -- set of symbols we can transit from f to t.
    addTr ((f, t):ps) dlt' | Just ss <- findTr dlt (Map.fromList eCl') f t =
        addTr ps $ Set.foldr (\s -> Map.alter (alt t) (f, Just s)) dlt' ss
      where
        -- (f, Just s) doesn't exists
        alt t Nothing = Just $ Set.singleton t
        alt t (Just st) = Just $ Set.insert t st
    addTr (_:ps) dlt' = addTr ps dlt'

-- Find symbol by which we can transit from f to ts.
findTr :: (Ord q, Ord s) => Delta q s -> Map q (Set q) -> q -> q -> Maybe (Set s)
findTr dlt eCl f t
  | Just transSts <- Map.lookup f eCl = --undefined
      fmap Set.fromList $ sequence $ filter isJust $ concatMap fn $ Set.toList transSts
  where
    -- dlt' contains transitions from trS to t
    fn trS = let dlt' = Map.filter (Set.member t) $ Map.filterWithKey (findPartKey trS) dlt
             in snd <$> fst <$> Map.toList dlt'
findTr _ _ _ _ = Nothing

eclose dlt sts st
  | not (st `Set.member` sts)
  , Just s <- Map.lookup (st, Nothing) dlt = let sts' = Set.insert st sts in Set.unions $ Set.map (eclose dlt sts') s
eclose dlt sts st = Set.insert st sts

-- Find all transitions from `st` state with each symbol.
findPartKey st (k, _) _ | st == k = True
findPartKey _ _ _ = False
