module PrettyAutomaton where

import AutomatonType

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

{-
pretty :: Automaton String (Set (State String)) -> Automaton String String
pretty (Automaton sig sts init term dlt) = Automaton sig sts' init' term' dlt'
  where
    sts'  = Set.map f sts
    init' = f init
    term' = Set.map f term
    dlt'  = Map.fromList ((\((q, s), qt) -> ((f q, s), Set.map f qt)) <$> Map.toList dlt)

    f (State q) = concat (Set.toList q)
-}

pretty = error "New version not yet implemented"
