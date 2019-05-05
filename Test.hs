module Main where

import           Data.Either
import           Data.List
import           System.Environment
import           Automaton
import           AutomatonType
import           Minimizer
import           Completer
import           Text.Printf
import           Determiner
import qualified Control.Exception             as Exc
import GHC.Exception
import System.IO.Unsafe
import Data.Maybe

{-# NOINLINE unsafeCleanup #-}
unsafeCleanup :: a -> Either String a
unsafeCleanup x = unsafePerformIO $ Exc.catch (x `seq` return (Right x)) handler
    where
    handler exc = return $ Left (displayException (exc :: Exc.ErrorCall))


checkMin a = unsafeCleanup $ isMinimal  $ minimize a 
checkCom a = unsafeCleanup $ isComplete $ complete a 
checkDet a = unsafeCleanup $ isDFA $ determine a 

checkAuto :: Automaton String String -> String 
checkAuto auto = 
  let show' = either id show in
  printf "Checks:\nMin: %s\nCom: %s\nDet: %s\n" (show' $ checkMin auto) (show' $ checkCom auto) (show' $ checkDet auto)

main :: IO ()
main = do

  fileNames <- getArgs
  mapM_
    (\fileName -> do
      input <- readFile fileName
      let a = parseAutomaton input
      putStrLn $ printf "Parsing %s\n" fileName
      putStrLn $ either (printf "Not an automaton!\n%s")
                        (checkAuto . fromJust)
                        a
      putStrLn ""
    )
    fileNames
