module Determiner {-(determine, isDFA)-} where

import AutomatonType

import Data.Maybe (catMaybes)

import Debug.Trace

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Sequence as Seq

type Seq = Seq.Seq

-- Checks if the automaton is deterministic (only one transition for each state and each input symbol)
isDFA :: Automaton a b -> Bool
isDFA (Automaton _ _ _ _ delta) = all check $ Map.toList delta
  where
    check ((_, Nothing), _) = False
    check ((_, Just _), sts) = Set.size sts == 1

determine :: (Ord q, Ord s) => Automaton s [q] -> Automaton s [q]
determine a | isDFA a = a
determine a = toSimple (determineCommon a)
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


newTerm' term sts = Set.filter (\s -> any id $ Set.toList $ Set.map (\t -> t `Set.member` (state s)) term) sts

initQueue' init = Seq.singleton (Set.singleton init)

determineCommon :: (Ord q, Ord s) => Automaton s q -> Automaton s (Set (State q))
determineCommon ~(Automaton sigs sts init term dlt) = Automaton sigs newSts (State newInit) newTerm newDlt
    where
      newInit = Set.singleton init
      -- Seq (Set (State q))
      initQueue = Seq.singleton newInit

      (newSts, newDlt) = go Set.empty initQueue Map.empty

      -- Td = { qd in Qd | exists p in T: p in qd }
      newTerm = Set.filter (\s -> any id $ Set.toList $ Set.map (\t -> t `Set.member` (state s)) term) newSts

      -- while queue != {}
      -- ret :: Set (State (Set (State q)))
      -- queue :: Seq (Set (State q))
      -- dltd :: Delta (State q) s = Maybe (State q, Maybe s) (Set (State q))
      go retSet queue dltd | Seq.null queue = (retSet, dltd)
      go retSet queue dltd =
        let (newDltd, newQueue, newRetSet) = Set.foldr f (dltd, queue'', retSet) sigs
        in go newRetSet newQueue newDltd
        where
          -- pd = queue.pop()
          -- pd :: Set (State q)
          pd = Seq.index queue 0
          queue'' = Seq.drop 1 queue

          f symb (dltd', queue', retSet') = let
              -- for p in pd:
              --   qd = qd \/ {dlt(p,c)}
              qd = State $ Set.foldr (\p st -> st `Set.union` (find p symb dlt)) Set.empty pd
              --   dltd(pd, qd) = c
              dltd'' = Map.insert (State pd, Just symb) (Set.singleton qd) dltd'
              in
              -- if qd not in Qd:
              --     P.push(qd)
              --     Qd.push(qd)
              if Set.null (state qd) || Set.member qd retSet'
              then (dltd'', queue', retSet')
              else (dltd'', (state qd) Seq.:<| queue', qd `Set.insert` retSet')

      find :: (Ord q, Ord s) => State q -> s -> Delta (State q) s -> Set (State q)
      find p s d = maybe Set.empty id (Map.lookup (p, Just s) d)
