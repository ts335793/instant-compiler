module SemanticAnalysis where

import BNFC.LexInstant
import BNFC.ParInstant
import BNFC.SkelInstant
import BNFC.PrintInstant
import BNFC.AbsInstant
import BNFC.ErrM

import Control.Monad.Error
import Control.Monad.State
import Data.Set

class SemanticallyAnalysable a where
  checkLabels :: a -> ErrorT String (State (Set Ident)) ()

instance SemanticallyAnalysable Program where
  checkLabels (Prog xs) = mapM_ checkLabels xs

instance SemanticallyAnalysable Stmt where
  checkLabels (SAss v e) = do
    checkLabels e
    lift $ modify $ insert v
  checkLabels (SExp e) = checkLabels e

instance SemanticallyAnalysable Exp where
  checkLabels (ExpAdd e1 e2) = checkLabels e1 >> checkLabels e2
  checkLabels (ExpSub e1 e2) = checkLabels e1 >> checkLabels e2
  checkLabels (ExpMul e1 e2) = checkLabels e1 >> checkLabels e2
  checkLabels (ExpDiv e1 e2) = checkLabels e1 >> checkLabels e2
  checkLabels (ExpLit i) = return ()
  checkLabels (ExpVar v) = do
    b <- lift $ member v <$> get
    unless b $ throwError $ "Unknown identifier: " ++ show v

runSemanticAnalysis :: Program -> Either String ()
runSemanticAnalysis p =
  case evalState (runErrorT (checkLabels p)) empty of
    Left e -> fail e
    Right () -> return ()