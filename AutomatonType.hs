module AutomatonType where

import qualified Data.Set as Set
import qualified Data.Map as Map


type Set = Set.Set
type Map = Map.Map

data State q = State q | Bot deriving (Show, Eq, Ord)

instance Functor State where
    fmap f Bot = Bot
    fmap f (State a) = State (f a)

state (State q) = q
state Bot = error "Cannot take value from Bot"

type Delta q s = Map (q, Maybe s) (Set q)
data Automaton s q = Automaton { sigma     :: Set s
                               , states    :: Set (State q)
                               , initState :: (State q)
                               , termState :: Set (State q)
                               , delta     :: Delta (State q) s
                               } deriving (Show, Eq)
