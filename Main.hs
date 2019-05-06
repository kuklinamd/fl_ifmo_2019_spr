module Main where

import System.Environment
import Expression
import Text.Printf
import Data.Char (isSpace)
import Optimizer
import Data.Either (isRight, fromRight)

main :: IO ()
main = do
  fileNames <- getArgs
  mapM_
    (\fileName -> do
        input <- trim <$> readFile fileName
        let a = parseExpression input
        putStrLn $ printf "Parsing %s\n" fileName
        putStrLn $ printf "Content: %s\n" input
        putStrLn "Warning: '-' is now unary minus, for substructon use '.-'."
        putStrLn $ either id show a
        putStrLn "Optimize:\n"
        putStrLn $ either id show (optimize <$> a)
        putStrLn ""
    )
    fileNames

trim xs = dropSpaceTail "" $ dropWhile isSpace xs

dropSpaceTail maybeStuff "" = ""
dropSpaceTail maybeStuff (x:xs)
        | isSpace x = dropSpaceTail (x:maybeStuff) xs
        | null maybeStuff = x : dropSpaceTail "" xs
        | otherwise = reverse maybeStuff ++ x : dropSpaceTail "" xs
