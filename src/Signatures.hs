module Signatures where

import Control.Monad.State

import Types

import Data.List
import qualified Data.Map as Map

import qualified Language.Python.Common.AST as AST

nothing :: a -> Maybe b
nothing _ = Nothing

_void :: Type
_void  = JustType "void"

bool :: Type
bool = JustType "boolean"

int :: Type
int = JustType "int"

float :: Type
float = JustType "float"

complex :: Type
complex = JustType "complex"

str :: Type
str = JustType "string"

bytes :: Type
bytes = JustType "bytes"

list :: Type -> Type
list t = List (Just t)

updateSignature :: Signature -> Signature -> Signature
updateSignature newSig sig arg =
    case newSig arg of
        Nothing -> sig arg
        res -> res

-- arithmetic signatures
defaultIntSignature :: Signature
defaultIntSignature arg =
    if arg == [bool, bool] then Just [int] else
    if arg == [bool, int] then Just [int] else
    if arg == [int, bool] then Just [int] else
    if arg == [int, int] then Just [int] else
        Nothing

defaultFloatSignature :: Signature
defaultFloatSignature arg =
    if arg == [bool, float] then Just [float] else
    if arg == [int, float] then Just [float] else
    if arg == [float, bool] then Just [float] else
    if arg == [float, int] then Just [float] else
    if arg == [float, float] then Just [float] else
        defaultIntSignature arg

defaultComplexSignature :: Signature
defaultComplexSignature arg =
    if arg == [bool, complex] then Just [complex] else
    if arg == [int, complex] then Just [complex] else
    if arg == [float, complex] then Just [complex] else
    if arg == [complex, bool] then Just [complex] else
    if arg == [complex, int] then Just [complex] else
    if arg == [complex, float] then Just [complex] else
    if arg == [complex, complex] then Just [complex] else
        defaultFloatSignature arg

defaultAdditionSignature :: Signature
defaultAdditionSignature = updateSignature sig' defaultComplexSignature
    where
        sig' :: Signature
        sig' arg = if arg == [str, str] then Just [str] else
            case arg of
                [List Nothing, List b] -> Just [(List b)]
                [List a, List Nothing] -> Just [(List a)]
                [List a, List b] -> if a == b then Just [(List a)] else Nothing
                _ -> Nothing

realDivisionSingature :: Signature
realDivisionSingature = updateSignature sig' defaultComplexSignature
    where
        sig' :: Signature
        sig' arg =
            if arg == [bool, bool] then Just [float] else
            if arg == [bool, int] then Just [float] else
            if arg == [int, bool] then Just [float] else
            if arg == [int, int] then Just [float] else
                Nothing

defaultComparisonSignature :: Signature
defaultComparisonSignature arg =
    if arg == [bool, bool] then Just [bool] else
    if arg == [bool, int] then Just [bool] else
    if arg == [bool, float] then Just [bool] else
    if arg == [int, bool] then Just [bool] else
    if arg == [int, int] then Just [bool] else
    if arg == [int, float] then Just [bool] else
    if arg == [float, bool] then Just [bool] else
    if arg == [float, int] then Just [bool] else
    if arg == [float, float] then Just [bool] else
    if arg == [str, str] then Just [bool] else
        case arg of
            [List a, List b] -> Just [bool]
            _ -> Nothing

defaultBinarySignature :: Signature
defaultBinarySignature arg =
    if arg == [bool, bool] then Just [bool] else
    if arg == [bool, int] then Just [int] else
    if arg == [int, bool] then Just [int] else
    if arg == [int, int] then Just [int] else
        Nothing

-- binary signatures
anythingFromArgs :: Int -> Signature
anythingFromArgs n args =
    if length args == n then
        Just args
    else
        Nothing

anythingTo :: Type -> Int -> Signature
anythingTo typ n args = 
    if length args == n then
        Just [typ]
    else
        Nothing

-- other signatures
printSig :: Signature
printSig _ = Just [_void]

typeSig :: Signature
typeSig [a] = Just [a]
typeSig _ = Nothing

rangeSig :: Signature
rangeSig args = 
    if args == [int] then Just [List (Just int)] else
    if args == [int, int] then Just [List (Just int)] else
    if args == [int, int, int] then Just [List (Just int)] else
        Nothing

infixr 4 `onesig`
onesig :: Argument -> Result -> Signature
newArg `onesig` newRes = (\arg -> if arg == newArg then Just newRes else Nothing)

-- operators into functions
startingOperators :: Operators
startingOperators = Map.fromList [("and", anythingFromArgs 2),
                                  ("or", anythingFromArgs 2),
                                  ("not", anythingTo bool 1),
                                  ("exp", defaultComplexSignature),
                                  ("lt", defaultComparisonSignature),
                                  ("gt", defaultComparisonSignature),
                                  ("eq", anythingTo bool 2),
                                  ("gte", defaultComparisonSignature),
                                  ("lte", defaultComparisonSignature),
                                  ("neq", anythingTo bool 2),
                                  ("in", iterable 2 (anythingTo bool)),
                                  ("is", anythingTo bool 2),
                                  ("isn", anythingTo bool 2),
                                  ("nin", iterable 2 (anythingTo bool)),
                                  ("bor", defaultBinarySignature),
                                  ("xor", defaultBinarySignature),
                                  ("band", defaultBinarySignature),
                                  ("sftl", defaultIntSignature),
                                  ("sftr", defaultIntSignature),
                                  ("mult", defaultComplexSignature),
                                  ("pls", defaultAdditionSignature),
                                  ("mns", defaultComplexSignature),
                                  ("div", realDivisionSingature),
                                  ("fdiv", defaultFloatSignature),
                                  ("matm", undefined),
                                  ("inv", anythingTo int 1),
                                  ("mod", defaultFloatSignature)]

getFunction :: AST.OpSpan -> TypeCheckerMonad Signature
getFunction op = do
    ops <- gets operators
    case op of
        (AST.And _)               -> return $ Map.findWithDefault nothing "and" ops
        (AST.Or _)                -> return $ Map.findWithDefault nothing "or" ops
        (AST.Not _)               -> return $ Map.findWithDefault nothing "not" ops
        (AST.Exponent _)          -> return $ Map.findWithDefault nothing "exp" ops
        (AST.LessThan _)          -> return $ Map.findWithDefault nothing "lt" ops
        (AST.GreaterThan _)       -> return $ Map.findWithDefault nothing "gt" ops
        (AST.Equality _)          -> return $ Map.findWithDefault nothing "eq" ops
        (AST.GreaterThanEquals _) -> return $ Map.findWithDefault nothing "gte" ops
        (AST.LessThanEquals _)    -> return $ Map.findWithDefault nothing "lte" ops
        (AST.NotEquals _)         -> return $ Map.findWithDefault nothing "neq" ops
        (AST.In _)                -> return $ Map.findWithDefault nothing "in" ops
        (AST.Is _)                -> return $ Map.findWithDefault nothing "is" ops
        (AST.IsNot _)             -> return $ Map.findWithDefault nothing "isn" ops
        (AST.NotIn _)             -> return $ Map.findWithDefault nothing "nin" ops
        (AST.BinaryOr _)          -> return $ Map.findWithDefault nothing "bor" ops
        (AST.Xor _)               -> return $ Map.findWithDefault nothing "xor" ops
        (AST.BinaryAnd _)         -> return $ Map.findWithDefault nothing "band" ops
        (AST.ShiftLeft _)         -> return $ Map.findWithDefault nothing "sftl" ops
        (AST.ShiftRight _)        -> return $ Map.findWithDefault nothing "sftr" ops
        (AST.Multiply _)          -> return $ Map.findWithDefault nothing "mult" ops
        (AST.Plus _)              -> return $ Map.findWithDefault nothing "pls" ops
        (AST.Minus _)             -> return $ Map.findWithDefault nothing "mns" ops
        (AST.Divide _)            -> return $ Map.findWithDefault nothing "div" ops
        (AST.FloorDivide _)       -> return $ Map.findWithDefault nothing "fdiv" ops
        (AST.MatrixMult _)        -> return $ Map.findWithDefault nothing "matm" ops
        (AST.Invert _)            -> return $ Map.findWithDefault nothing "inv" ops
        (AST.Modulo _)            -> return $ Map.findWithDefault nothing "mod" ops

-- auxiliary functions
iterable :: Int -> (Int -> Signature) -> Signature
iterable n sigN args =
    if isIterable (last args) then
        sigN n args
    else
        Nothing
    where
        isIterable :: Type -> Bool
        isIterable (Tuple _) = True
        isIterable (List _) = True
        isIterable _ = False