module JVM where

import BNFC.LexInstant
import BNFC.ParInstant
import BNFC.SkelInstant
import BNFC.PrintInstant
import BNFC.AbsInstant
import BNFC.ErrM

import Data.Map hiding (map, filter, foldr)
import Control.Monad.State

data Instruction = Add
                 | Sub
                 | Mul
                 | Div
                 | Swap
                 | Const Integer
                 | Load Integer
                 | Store Integer
                 | LoadPrint
                 | Print
                 | BoilerplateCode String
                 | BeginMainDeclaration
                 | LimitStack Integer
                 | LimitLocals Integer
                 | Return
                 | EndMainDeclaration

instance Show Instruction where
  show Add = "iadd"
  show Sub = "isub"
  show Mul = "imul"
  show Div = "idiv"
  show Swap = "swap"
  show (Const i)
    | i == -1 = "iconst_m1"
    | 0 <= i && i <= 5 = "iconst_" ++ show i
    | -128 <= i && i <= 127 = "bipush " ++ show i
    | -32768 <= i && i <= 32767 = "sipush " ++ show i
    | otherwise = "ldc " ++ show i
  show (Load i)
    | 0 <= i && i <= 3 = "iload_" ++ show i
    | otherwise = "iload " ++ show i
  show (Store i)
    | 0 <= i && i <= 3 = "istore_" ++ show i
    | otherwise = "istore " ++ show i
  show LoadPrint = "getstatic java/lang/System/out Ljava/io/PrintStream;"
  show Print = "invokevirtual java/io/PrintStream/println(I)V"
  show (BoilerplateCode s) = ".class  public " ++ s ++ "\n\
                             \.super  java/lang/Object\n\
                             \.method public <init>()V\n\
                             \aload_0\n\
                             \invokespecial java/lang/Object/<init>()V\n\
                             \return\n\
                             \.end method"
  show BeginMainDeclaration = ".method public static main([Ljava/lang/String;)V"
  show (LimitStack i) = ".limit stack " ++ show i
  show (LimitLocals i) = ".limit locals " ++ show i
  show Return = "return"
  show EndMainDeclaration = ".end method"

data CompilerState = CompilerState { nextLocal :: Integer
                                   , varToLocal :: Map Ident Integer
                                   }

bind :: Ident -> Integer -> State CompilerState ()
bind v i = modify $ \ s -> CompilerState { nextLocal = nextLocal s, varToLocal = insert v i $ varToLocal s }

getLocal :: Ident -> State CompilerState Integer
getLocal v = flip (!) v . varToLocal <$> get

newLocal :: State CompilerState Integer
newLocal = do
  i <- nextLocal <$> get
  modify $ \ s -> CompilerState { nextLocal = nextLocal s + 1, varToLocal = varToLocal s }
  return i

stackSize :: Integer -> Integer -> Integer
stackSize a b = min (max a (b + 1)) (max (a + 1) b)

compileExpHelp :: Exp -> Exp -> Instruction -> Bool -> State CompilerState (Integer, [Instruction] -> [Instruction])
compileExpHelp e1 e2 const commutative = do
  (d1, i1) <- compileExp e1
  (d2, i2) <- compileExp e2
  let d = stackSize d1 d2
  if d1 >= d2 then return (d, i1 . i2 . ([const] ++))
  else if commutative then return (d, i2 . i1 . ([const] ++))
  else return (d, i2 . i1 . ([Swap, const] ++))

compileExp :: Exp -> State CompilerState (Integer, [Instruction] -> [Instruction])
compileExp (ExpAdd e1 e2) = compileExpHelp e1 e2 Add True
compileExp (ExpSub e1 e2) = compileExpHelp e1 e2 Sub False
compileExp (ExpMul e1 e2) = compileExpHelp e1 e2 Mul True
compileExp (ExpDiv e1 e2) = compileExpHelp e1 e2 Div False
compileExp (ExpLit i) = return (1, ([Const i] ++))
compileExp (ExpVar v) = do
  i <- getLocal v
  return (1, ([Load i] ++))

compileStmt :: Stmt -> State CompilerState (Integer, [Instruction] -> [Instruction])
compileStmt (SAss v e) = do
  (d, ie) <- compileExp e
  i <- newLocal
  bind v i
  return (d, ie . ([Store i] ++))
compileStmt (SExp e) = do
  (d, ie) <- compileExp e
  return (max d 2, ie . ([LoadPrint, Swap, Print] ++))

compileProgram :: Program -> String -> State CompilerState [Instruction]
compileProgram (Prog xs) name = do
  rs <- mapM compileStmt xs
  let d = maximum $ map fst rs
  let is = foldr snd [] rs
  l <- nextLocal <$> get
  return $ [BoilerplateCode name, BeginMainDeclaration, LimitStack d, LimitLocals l] ++ is ++ [Return, EndMainDeclaration]
  where
    isSAss (SAss _ _) = True
    isSAss _ = False

compile :: Program -> String -> [Instruction]
compile p name = evalState (compileProgram p name) CompilerState { nextLocal = 1, varToLocal = empty }