module Main where

import Tokenizer

runTokenizer :: String -> IO ()
runTokenizer input = do
  putStrLn $ "`" ++ input ++ "'"
  putStrLn $ show $ tokenize input
  putStrLn ""

main :: IO ()
main = do
  runTokenizer " 1 2 abc if "
  runTokenizer " "
  runTokenizer "if a 0x10 else b123 0XFA"
  runTokenizer " "
  runTokenizer "abc a1a ab23"
  runTokenizer "10"
