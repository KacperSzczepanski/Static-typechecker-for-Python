{-# LANGUAGE InstanceSigs #-}

module Types where

import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List

import qualified Language.Python.Common.SrcLocation as SL
import qualified Language.Python.Common.AST as AST

type Name = String

-- return state to track if all branches of functions have return
data RetState = NoRet -- no return found yet in the current branch of code
              | RetDone [Type] -- return found and it covers all paths, just checking the rest (do we need it...?)
              | RetTBD [Type] -- some paths found return, but possible results can still be encountered
              deriving (Eq, Show)

data Type = JustType Name -- simplest types
          | Function Signature -- taken from Signature.hs
          | Tuple [Type]
          | AnyTuple -- tuples which we know nothing about whats inside 
          | List (Maybe Type) -- just a type of contents
          | Class Name ([Type], Int) -- class' name with types of its initialization parameters (and id needed for some computations)

instance Show Type where
    show (JustType str) = str
    show (Function sig) = "<function>"
    show (Tuple list) = "(" ++ intercalate ", " (map show list) ++ ")"
    show AnyTuple = "(*)"
    show (List Nothing) = "List <>"
    show (List (Just t)) = "List <" ++ show t ++ ">"
    show (Class name (types, num)) = name ++ " " ++ show types 

instance Eq Type where
    (JustType a) == (JustType b) = a == b
    (Function a) == (Function b) = False
    (Tuple l1) == (Tuple l2) = l1 == l2
    AnyTuple == AnyTuple = True
    (List l1) == (List l2) = l1 == l2
    (Class n1 ts1) == (Class n2 ts2) = (n1 == n2 && ts1 == ts2)
    a == b = False

type Argument = [Type]
type Result = [Type]
type Signature = Argument -> Maybe Result -- tuple of args -> tuple of results
type Operators = Map.Map Name Signature

type Env = Map.Map Name Type -- environment of variables

type Inferred = [(Name, SL.SrcSpan, Type)] -- inferred informations
type Run = (Env, Inferred)
    
-- class informations
type Store = Map.Map Name (Env, Env) -- type name, then left attributes, right methods
type CInferred = (Name, SL.SrcSpan, [(([Type], Int), Inferred, FInferred)]) -- list of inferred informations for each tuple of argument types

type ParamMap = Map.Map Name [AST.ArgumentSpan] -- new expressions to add to function as parameters when calling
type ExtraParams = [AST.ArgumentSpan]
type FEnv = Map.Map Name (Signature, ExtraParams)
type AppType = ([Type], [Type])
type FInferred = [(Name, SL.SrcSpan, [AppType])]

type ClassGraph = Map.Map Name (Name, Maybe AST.StatementSpan, Maybe AST.StatementSpan, [AST.StatementSpan]) -- parent class, init, add, other methods

-- first argument for class inference, whether analyzed part is inside function, store of types, signatures of operators, runs, functions environment (for handling unknown variables in functions), inferred informations for functions, collected unknown variables
data TCState = TCState {firstArg :: Maybe (Name, Type), inFunction :: Bool, store :: Store, operators :: Operators, runs :: [Run], functions :: FEnv, fInferred :: FInferred, newParameters :: [Name]}
initTCState :: Maybe (Name, Type) -> Bool -> Store -> Operators -> [Run] -> FEnv -> FInferred -> [Name] -> TCState
initTCState fA inFun st ops runs fenv finf np = TCState {firstArg = fA, inFunction = inFun, store = st, operators = ops, runs = runs, functions = fenv, fInferred = finf, newParameters = np}

type TypeCheckerMonad a = StateT TCState (Except String) a
runTypeCheckerMonad :: TCState -> TypeCheckerMonad a -> Either String (a, TCState)
runTypeCheckerMonad st ev = runExcept (runStateT ev st)