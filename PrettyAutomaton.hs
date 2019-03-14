module PrettyAutomaton where

import AutomatonType

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

pretty :: (Ord q, Ord s) => Automaton s (Set [q]) -> Automaton s [q]
pretty (Automaton sig sts init term dlt) = Automaton sig sts' init' term' dlt'
  where
    sts' = Set.map to sts
    init' = to init
    term' = Set.map to term
    dlt' = Map.fromList ((\((q, s), qt) -> ((to q, s), Set.map to qt)) <$> Map.toList dlt)

    to = mconcat . Set.toList
