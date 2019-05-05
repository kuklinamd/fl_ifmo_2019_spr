module Main where

import System.Environment
import Expression
import Text.Printf
import Data.Char (isSpace)

main :: IO ()
main = do
  fileNames <- getArgs
  mapM_
    (\fileName -> do
        input <- trim <$> readFile fileName
        let a = parseExpression input
        let r = executeExpression input
        putStrLn $ printf "Parsing %s\n" fileName
        putStrLn $ either id show a
        putStrLn $ either id show r
        putStrLn ""
    )
    fileNames

trim xs = dropSpaceTail "" $ dropWhile isSpace xs

dropSpaceTail maybeStuff "" = ""
dropSpaceTail maybeStuff (x:xs)
        | isSpace x = dropSpaceTail (x:maybeStuff) xs
        | null maybeStuff = x : dropSpaceTail "" xs
        | otherwise = reverse maybeStuff ++ x : dropSpaceTail "" xs
