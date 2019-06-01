module Main where

import System.Environment
import Text.Printf
import Ast
import Parser
import ParserCombinators
import Control.Monad
import Data.Either

main :: IO ()
main = do
  fileNames <- getArgs
  forM_ fileNames $ \fileName -> do
    putStrLn $ ">>> Handle " ++ fileName ++ "\n\n"
    input <- readFile fileName
    let simpl = runParser (parseText <* eof) input
    let nested = runParser (parseTextNested <* eof) input

    isFail simpl "Failed to parse file without nested multiline comments."
    isFail nested "Failed to parse file with nested multiline comments."

    if isLeft nested
    then
       putStrLn "Stop handle the file."
    else
        case programParser (getParsed nested) of
            Left err -> putStrLn $ "error: " ++ err
            Right (_, ast) -> do
              putStrLn $ "\n>>> Original:\n"
              putStrLn input
              putStrLn "\n>>> Pretty print of parsed AST:\n"
              putStrLn $ pretty ast
              -- putStrLn "\n>>> Type checker result (context):\n"
              -- case typeCheckerCtx ast of
              --   Left err -> putStrLn $ "error: " ++ err
              --   Right ctx -> do
              --       putStrLn "Everything typechecked."
              --       putStrLn $ pretty ctx

isFail (Right _) _ = pure ()
isFail (Left _) str = putStrLn str
