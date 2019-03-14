module AutomatonType where

import qualified Data.Set as Set
import qualified Data.Map as Map


type Set = Set.Set
type Map = Map.Map

type Delta q s = Map (q, Maybe s) (Set q)
data Automaton s q = Automaton { sigma     :: Set s
                               , states    :: Set q
                               , initState :: q
                               , termState :: Set q
                               , delta     :: Delta q s
                               } deriving (Show, Eq)
