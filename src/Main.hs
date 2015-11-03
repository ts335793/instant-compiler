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
  let s = "a = 2*2; 13 * a; b = a + 2; b + 2 + a; a = 10; 1 - (2 + 3 * a)"
  case parse s of
    Left e -> putStrLn e
    Right t ->
      case runSemanticAnalysis t of
        Left e -> putStrLn e
        Right () -> do
          mapM_ print (LLVM.compile t)
          mapM_ print (JVM.compile t)