module Completer where

import AutomatonType

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Data.Maybe (fromJust)

import Debug.Trace


complete :: (Ord s, Ord q, Show q, Show s) => Automaton s q -> Automaton s q
complete (Automaton sig sts init term delt) = Automaton sig sts' init term' delt'
  where
    sts'  = Set.insert Bot sts
    term' = Set.insert Bot term
    delt' = Set.foldr (\s dl -> f s dl) delt sts

    f st dlt = let
        mp = Map.filterWithKey (findPartKey st) dlt
      in if null mp
         -- if there's no transitions for the key, add transitions to the Bot.
         then new sig st dlt
         -- We have at least one transition for the state.
         -- Add to the delta the rest transitions
         -- fromJust -- we have there DFA
         -- snd -- we
         else add st (getSymbols sig mp) dlt --

getSymbols sig mp = let a = snd <$> Map.keys mp
                in filter (\s -> not $ elem (Just s) a) (Set.toList sig)

new sig st dlt = Set.foldr (\s d -> Map.insert (st, Just s) (Set.singleton Bot) d) dlt sig

add a ss dlt = foldr (\s -> Map.insert (a, Just s) (Set.singleton Bot)) dlt ss

-- Find all transitions from `st` state with each symbol.
findPartKey st (k, _) _ | st == k = True
findPartKey _ _ _ = False
