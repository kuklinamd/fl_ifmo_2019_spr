module Main where

import System.Environment
import Text.Printf
import Ast
import Parser
import TypeChecker
import ParserCombinators

main :: IO ()
main = do
  fileNames <- getArgs
  mapM_
    (\fileName -> do
        input <- readFile fileName
        case programParser input of
            Left err -> putStrLn $ "error: " ++ err
            Right (_, ast) -> do
              putStrLn "\n>>> Pretty print of parsed AST:\n"
              putStrLn $ pretty ast
              putStrLn "\n>>> Type checker result (context):\n"
              case typeCheckerCtx ast of
                Left err -> putStrLn $ "error: " ++ err
                Right ctx -> do
                    putStrLn "Everything typechecked."
                    putStrLn $ pretty ctx
    )
    fileNames
