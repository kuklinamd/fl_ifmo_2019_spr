module Main where

import System.Environment
import AutomatonType
import Automaton
import Determiner
import Completer
import Minimizer
import Text.Printf

automatonInfo :: (Ord a, Ord b) => Maybe (Automaton a b) -> String
automatonInfo Nothing = "Not an automaton!\n"
automatonInfo (Just auto) =
  let [dfa, nfa, complete, minimal] = map (\f -> if f auto then "yes" else "no") [isDFA, isNFA, isComplete, isMinimal] in
  printf "Hurray! It's an automaton!\nDeterministic:    %s\nNondeterministic: %s\nComplete:         %s\nMinimal:          %s" dfa nfa complete minimal

main :: IO ()
main = do
  fileNames <- getArgs
  mapM_
    (\fileName -> do
        input <- readFile fileName
        let a = parseAutomaton input
        putStrLn $ printf "Parsing %s\n" fileName
        putStrLn $ either (printf "Not an automaton!\n%s") automatonInfo a
        putStrLn ""
    ) 
    fileNames
