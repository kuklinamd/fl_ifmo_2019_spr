module Determiner (determine, isDFA) where

import AutomatonType

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- Checks if the automaton is deterministic (only one transition for each state and each input symbol)
isDFA :: Automaton a b -> Bool
isDFA (Automaton _ _ _ _ delta) = all check $ Map.toList delta
  where
    check ((_, Nothing), _) = False
    check ((_, Just _), sts) = Set.size sts == 1

determine :: (Ord q, Ord s) => Automaton s q -> Automaton s q
determine a | isDFA a = a
