module Completer (complete, isComplete) where

import AutomatonType

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Data.Maybe (fromJust, isJust)

import Debug.Trace

-- Checks if the automaton is complete (there exists a transition for each state and each input symbol)
isComplete :: (Ord a, Ord b) => Automaton a b -> Bool
isComplete a@(Automaton sig sts _ _ delta) | not (isNFAUsefull a) = all check ((,) <$> Set.toList sts <*> Set.toList sig)
  where
    check (sig, st) = isJust $ Map.lookup (sig, Just st) delta

    isNFAUsefull :: Automaton a b -> Bool
    isNFAUsefull a@(Automaton _ _ _ _ delta) = any check (Map.toList delta)
      where
        check ((_, Nothing), _) = True
        check ((_, Just _), sts) = Set.size sts > 1
isComplete _ = False

complete :: (Ord s, Ord q, Show q, Show s) => Automaton s q -> Automaton s q
complete a | isComplete a = a
complete (Automaton sig sts init term delt) = Automaton sig sts' init term' delt'
  where
    sts'  = Set.insert Bot sts
    term' = Set.insert Bot term
    delt' = Set.foldr (\s dl -> f s dl) delt sts'

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
