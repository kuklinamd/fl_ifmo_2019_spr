module Main where
    
import System.Environment
import Expression
import Text.Printf
import Data.Char
import Data.List

main :: IO ()
main = do
  fileNames <- getArgs
  mapM_
    (\fileName -> do
        input <- readFile fileName
        let a = parseExpression $ trim input
        putStrLn $ printf "Parsing %s\n" fileName
        putStrLn $ either id show a
        putStrLn ""
    )
    fileNames


trim xs = dropSpaceTail "" $ dropWhile isSpace xs

dropSpaceTail maybeStuff "" = ""
dropSpaceTail maybeStuff (x:xs)
        | isSpace x = dropSpaceTail (x:maybeStuff) xs
        | null maybeStuff = x : dropSpaceTail "" xs
        | otherwise       = reverse maybeStuff ++ x : dropSpaceTail "" xs
