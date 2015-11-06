module LLVM where

import BNFC.LexInstant
import BNFC.ParInstant
import BNFC.SkelInstant
import BNFC.PrintInstant
import BNFC.AbsInstant
import BNFC.ErrM

import Control.Monad.State
import Control.Applicative hiding (empty)
import Data.Map

data Value = Register Integer
           | Constant Integer

instance Show Value where
  show (Register i) = "%t" ++ show i
  show (Constant i) = show i

data Instruction = Add Value Value Value
                 | Sub Value Value Value
                 | Mul Value Value Value
                 | Div Value Value Value
                 | DeclarePrintInt
                 | BeginMainDeclaration
                 | CallPrintInt Value
                 | Return Value
                 | EndMainDeclaration

instance Show Instruction where
  show (Add r v1 v2) = show r ++ " = add i32 " ++ show v1 ++ ", " ++ show v2
  show (Sub r v1 v2) = show r ++ " = sub i32 " ++ show v1 ++ ", " ++ show v2
  show (Mul r v1 v2) = show r ++ " = mul i32 " ++ show v1 ++ ", " ++ show v2
  show (Div r v1 v2) = show r ++ " = sdiv i32 " ++ show v1 ++ ", " ++ show v2
  show DeclarePrintInt = "declare void @printInt(i32)"
  show BeginMainDeclaration = "define i32 @main() {"
  show (CallPrintInt v) = "call void @printInt(i32 " ++ show v ++ ")"
  show (Return v) = "ret i32 " ++ show v
  show EndMainDeclaration = "}"

data CompilerState = CompilerState { nextRegister :: Integer
                                   , varToValue :: Map Ident Value
                                   , instructions :: [Instruction]
                                   }
  deriving Show

newRegister :: State CompilerState Value
newRegister = do
  r <- nextRegister <$> get
  modify $ \ s -> CompilerState { nextRegister = nextRegister s + 1, varToValue = varToValue s, instructions = instructions s }
  return $ Register r

emit :: Instruction -> State CompilerState ()
emit i = modify $ \ s -> CompilerState { nextRegister = nextRegister s, varToValue = varToValue s, instructions = i : instructions s }

bind :: Ident -> Value -> State CompilerState ()
bind v r = modify $ \s -> CompilerState { nextRegister = nextRegister s, varToValue = insert v r $ varToValue s, instructions = instructions s }

getValue :: Ident -> State CompilerState Value
getValue v = flip (!) v . varToValue <$> get

compileExpHelp :: Exp -> Exp -> (Value -> Value -> Value -> Instruction) -> State CompilerState Value
compileExpHelp e1 e2 const = do
  v1 <- compileExp e1
  v2 <- compileExp e2
  r <- newRegister
  emit $ const r v1 v2
  return r

compileExp :: Exp -> State CompilerState Value
compileExp (ExpAdd e1 e2) = compileExpHelp e1 e2 Add
compileExp (ExpSub e1 e2) = compileExpHelp e1 e2 Sub
compileExp (ExpMul e1 e2) = compileExpHelp e1 e2 Mul
compileExp (ExpDiv e1 e2) = compileExpHelp e1 e2 Div
compileExp (ExpLit l) = return $ Constant l
compileExp (ExpVar v) = getValue v

compileStmt :: Stmt -> State CompilerState ()
compileStmt (SAss v e) = bind v =<< compileExp e
compileStmt (SExp e) = emit . CallPrintInt =<< compileExp e

compileProgram :: Program -> State CompilerState ()
compileProgram (Prog xs) = do
  emit DeclarePrintInt
  emit BeginMainDeclaration
  mapM_ compileStmt xs
  emit $ Return $ Constant 0
  emit EndMainDeclaration

compile :: Program -> [Instruction]
compile p =
  let s = execState (compileProgram p) CompilerState { nextRegister = 0, varToValue = empty, instructions = [] }
  in reverse $ instructions s