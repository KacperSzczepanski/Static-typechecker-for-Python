module Store where

import Types
import Signatures

import qualified Data.Map as Map

store = addTuple $ addList $ addString $ addComplex $ addFloat $ addInteger $ addBoolean Map.empty

initialFEnv :: FEnv
initialFEnv = Map.empty

addTuple :: Store -> Store
addTuple = Map.insert ".tuple" (attrs, methods) where
    attrs = Map.empty
    methods = Map.empty


addList :: Store -> Store
addList = Map.insert ".list" (attrs, methods) where
    attrs = Map.empty
    methods = Map.fromList [("length", Function lenSig), ("append", Function appSig)]

    lenSig :: Signature
    lenSig arg = case arg of
        [List _] -> Just [int]
        _ -> Nothing

    appSig :: Signature
    appSig arg = case arg of
        [List Nothing, t] -> Just [(List (Just t))]
        [List (Just t1), t2] -> if t1 == t2 then Just [(List (Just t1))] else Nothing
        _ -> Nothing

addInteger :: Store -> Store
addInteger = Map.insert ".int" (attrs, methods) where
    attrs = Map.empty
    methods = Map.empty

addFloat :: Store -> Store
addFloat = Map.insert ".float" (attrs, methods) where
    attrs = Map.empty
    methods = Map.empty

addComplex :: Store -> Store
addComplex = Map.insert ".complex" (attrs, methods) where
    attrs = Map.empty
    methods = Map.empty

addBoolean :: Store -> Store
addBoolean = Map.insert ".boolean" (attrs, methods) where
    attrs = Map.empty
    methods = Map.empty

addString :: Store -> Store
addString = Map.insert ".string" (attrs, methods) where
    attrs = Map.empty
    methods = Map.empty

typeIdentifier :: Type -> String
typeIdentifier (JustType t) = show t
typeIdentifier (Function f) = ".func"
typeIdentifier (Tuple ts) = ".tuple"
typeIdentifier (AnyTuple) = ".tuple"
typeIdentifier (List t) = ".list"
typeIdentifier (Class n pair@(ts, num)) = show (length ts) ++ " " ++ n ++ " " ++ show pair

basicTypes :: [Type]
basicTypes = [_void, bool, int, float, complex, str]

idToType :: String -> [Type]
idToType id = 
    if id ==  ".void" then [_void] else
    if id == ".boolean" then [bool] else
    if id == ".int" then [int] else
    if id == ".float" then [float] else
    if id == ".complex" then [complex] else
    if id == ".string" then [str] else
    if id == ".func" then [] else
    if id == ".tuple" then [AnyTuple] else
    if id == ".list" then allListsWithDepthUpTo basicTypes 2 else 
    if take 6 id == "class " then
        [Class "" ([], 0)]
    else []
    where
        allListsWithDepthUpTo :: [Type] -> Int -> [Type]
        allListsWithDepthUpTo picks 0 = [List Nothing]
        allListsWithDepthUpTo picks 1 = [List Nothing] ++ (map (\typ -> List (Just typ)) picks)
        allListsWithDepthUpTo picks n = let res' = tail $ allListsWithDepthUpTo picks (n - 1) in
            [List Nothing] ++ res' ++ (map (\typ -> List (Just typ)) res')