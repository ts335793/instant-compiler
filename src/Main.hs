module Main where

import BNFC.LexInstant
import BNFC.ParInstant
import BNFC.SkelInstant
import BNFC.PrintInstant
import BNFC.AbsInstant
import BNFC.ErrM

import Control.Monad
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Data.Map as M
import Data.Set as S
import System.Environment

import LLVM
import JVM
import SemanticAnalysis

parse :: String -> Either String Program
parse s =
  case pProgram $ myLexer s of
    Bad e -> Left e
    Ok t -> Right t

main :: IO ()
main = do
  [compiler, srcPath] <- getArgs
  src <-readFile srcPath
  case parse src of
    Left e -> putStrLn e
    Right ast ->
      case runSemanticAnalysis ast of
        Left e -> putStrLn e
        Right () -> do
          if compiler == "llvm" then mapM_ print (LLVM.compile ast)
          else if compiler == "jvm" then mapM_ print (JVM.compile ast)
          else putStrLn $ "Unknown compiler " ++ compiler