module Trie(
    Trie(..)
    , empty
    , insert
    , next
    , fromList
)
where
    
import qualified Data.Map.Strict as Map

data Trie = Trie { children :: Map.Map Char Trie, isTerminal :: Bool }
  deriving Show


empty :: Trie
empty = Trie Map.empty False

insert :: Trie -> String -> Trie
insert (Trie m _) [] = (Trie m True)
insert (Trie m term) (s:ss) =
  case Map.lookup s m of
    Nothing -> Trie (Map.insert s (insert empty ss) m) term
    Just t' -> Trie (Map.insert s (insert t' ss) m) term

next :: Trie -> Char -> Maybe Trie
next t c = Map.lookup c (children t)

fromList :: [String] -> Trie
fromList [] = empty
fromList (s:ss) = insert (fromList ss) s
