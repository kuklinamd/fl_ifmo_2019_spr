module Completer where

import AutomatonType

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Data.Maybe (fromJust)

import Debug.Trace


complete :: (Ord s, Ord q, Show q, Show s) => Automaton s q -> Automaton s q
complete (Automaton sig sts init term delt) = Automaton sig sts' init term' delt'
  where
    sts' = Set.insert Bot sts
    term' = Set.insert Bot term
    delt' = Set.foldr (\s dl -> f s dl) delt sts

    f st dlt = let
        mp = Map.filterWithKey (findPartKey st) dlt
      in if null mp then new st dlt else add st ((fromJust . snd) <$> Map.keys mp) dlt

    new st dlt = Set.foldr (\s d -> Map.insert (st, Just s) (Set.singleton Bot) d) dlt sig

    add a mp dlt = trace (show a ++ " " ++ show mp) dlt

-- Find all transitions from `st` state with each symbol.
findPartKey st (k, _) _ | st == k = True
findPartKey _ _ _ = False
