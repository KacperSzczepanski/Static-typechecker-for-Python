module Utils where

import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List
import Data.Maybe
import Data.Char

import Types
import Signatures
import qualified Store as Store

import qualified Language.Python.Common.SrcLocation as SL
import qualified Language.Python.Common.AST as AST

-- auxiliary output functions
spanShow :: SL.SrcSpan -> String
spanShow (SL.SpanCoLinear file row col_start col_end) = file ++ ":" ++ show row ++ ":" ++ show col_start ++ "-" ++ show col_end
spanShow (SL.SpanMultiLine file row_start col_start row_end col_end) = file ++ ":" ++ show row_start ++ "-" ++ show row_end ++ ":" ++ show col_start ++ "-" ++ show col_end 
spanShow (SL.SpanPoint file row col) = file ++ ":" ++ show row ++ ":" ++ show col
spanShow SL.SpanEmpty = ""

errMessage :: SL.SrcSpan -> String -> String
errMessage span err = spanShow span ++ ": " ++ err

formatError :: SL.SrcSpan -> String -> TypeCheckerMonad a
formatError span err = throwError (errMessage span err)

srcWithoutFile :: SL.SrcSpan -> SL.SrcSpan
srcWithoutFile (SL.SpanCoLinear file row col_start col_end) = (SL.SpanCoLinear "" row col_start col_end)
srcWithoutFile (SL.SpanMultiLine file row_start col_start row_end col_end) = (SL.SpanMultiLine "" row_start col_start row_end col_end)
srcWithoutFile (SL.SpanPoint file row col) = (SL.SpanPoint "" row col)
srcWithoutFile SL.SpanEmpty = SL.SpanEmpty

blankAnnot :: SL.SrcSpan
blankAnnot = SL.SpanEmpty

-- update functions
-- update return with new results
updateReturn :: RetState -> [Type] -> RetState
updateReturn NoRet types = RetDone types
updateReturn ret@(RetDone _) _ = ret
updateReturn (RetTBD typs) types = RetDone (typs ++ types)

-- update all runs with their respective types
updateRuns :: (Name, SL.SrcSpan) -> [[Type]] -> (Name, [AST.ArgumentSpan]) -> [Run] -> [Run] -> [Run]
updateRuns _ [] _ [] acc = acc
updateRuns var@(name, src) (types : rt) (funname, newparams) ((env, inf) : rr) acc =
    let runs = map (\typ -> (Map.insert name typ env, inf ++ [(name, src, typ)])) types in
    updateRuns var rt (funname, newparams) rr (acc ++ runs)

-- runs to string, uncomment these 3 lines for entire output showcase
prettyInferenceString :: [CInferred] -> FInferred -> [Run] -> String
prettyInferenceString cinf finf runs = --"CLASSES\n\n" ++ prettyClassesString cinf ++ 
                                       --"\n\nFUNCTIONS\n\n" ++ prettyFunctionsString finf ++ 
                                       --"\n\n" ++ 
                                       prettyMainInferenceString runs

prettyClassesString :: [CInferred] -> String
prettyClassesString [] = ""
prettyClassesString ((name, src, packs) : rest) = "NAME: " ++ name ++ " (" ++ (tail $ spanShow (srcWithoutFile src)) ++ ")\n\n" ++ 
                                                  concatMap printPack packs ++
                                                  "\n" ++ prettyClassesString rest
    where
        printPack :: (([Type], Int), Inferred, FInferred) -> String
        printPack (args, infs, finfs) = "constructor arguments: " ++ show args ++
                                        "\nattributes:\n" ++ prettyInferred infs ++
                                        "methods:\n" ++ prettyFunctionsString finfs

prettyFunctionsString :: FInferred -> String
prettyFunctionsString = prettyInferred
    where
        prettyInferred :: FInferred -> String
        prettyInferred [] = ""
        prettyInferred ((name, src, typ) : rest) = 
            name ++ "(" ++ (tail $ spanShow (srcWithoutFile src)) ++ ")\n" ++ appsToString typ
            ++ "\n\n" ++ prettyInferred rest

        appsToString :: [AppType] -> String
        appsToString [] = ""
        appsToString ((arg, res) : rest) = (intercalate ", " (map show arg) ++ " -> " ++ intercalate ", " (map show res)) ++
                                        if rest == [] then
                                            ""
                                        else
                                            "\n" ++ appsToString rest

prettyMainInferenceString :: [Run] -> String
prettyMainInferenceString [] = "NO POSSIBLE RUNS"
prettyMainInferenceString runs = "ALL POSSIBLE RUNS (" ++ show (length runs) ++ ")\n" ++ runToString runs 1
    where
        runToString :: [Run] -> Int -> String
        runToString [] _ = ""
        runToString ((_, infers) : rest) n = 
            "------------------------------\nRUN " ++ (show n) ++ "\n" ++ prettyInferred infers
            ++ runToString rest (n + 1)
        
prettyInferred :: Inferred -> String
prettyInferred [] = ""
prettyInferred ((name, src, typ) : rest) =
    name ++ "(" ++ (tail $ spanShow (srcWithoutFile src)) ++ ")" ++ " " ++ show typ
    ++ "\n" ++ prettyInferred rest

-- create all combinations of choosing exactly 1 element from each list
-- [[a, b], [c, d], [e]] -> [[a, c, e], [a, d, e], [b, c, e], [b, d, e]]
allChoices :: (Eq a) => [[a]] -> [[a]]
allChoices list = if [] `elem` list || list == [] then [[]] else allChoices' list
    where
        allChoices' :: [[a]] -> [[a]]
        allChoices' [] = []
        allChoices' [l] = foldr (\a b -> [a] : b) [] l
        allChoices' (l : rest) = go l (allChoices' rest) 
            where
            go :: [a] -> [[a]] -> [[a]]
            go [] _ = []
            go (x : xs) base = map (x :) base ++ go xs base

-- get all possible types
domain :: Env -> TypeCheckerMonad [Type]
domain env = do
    types <- getAllTypesUpToLevel 3
    return $ (List Nothing : types) ++ [AnyTuple]
    where
        filterClasses :: Type -> TypeCheckerMonad Bool
        filterClasses cl@(Class name types) = do
            st <- gets store
            case Map.lookup (Store.typeIdentifier cl) st of
                Nothing -> return False
                _ -> return True
        filterClasses _ = return True

        getAllTypesUpToLevel :: Int -> TypeCheckerMonad [Type]
        getAllTypesUpToLevel 0 = return [_void] -- nothing
        getAllTypesUpToLevel 1 = return [bool, int, float, complex, str] -- simple types
        getAllTypesUpToLevel n = do -- complex types, created by applying them to another type or types
            types <- getAllTypesUpToLevel (n - 1)
            let res' = map (\typ -> List (Just typ)) types
            res'' <- toClasses types
            return $ nub $ types ++ res' ++ res''

        toClasses :: [Type] -> TypeCheckerMonad [Type]
        toClasses types = do
            cl <- classes
            let res = concatMap (toClass types) cl
            return res
            where
                toClass :: [Type] -> (Int, Name) -> [Type]
                toClass types (n, cl) = 
                    case Map.lookup cl env of
                        Just (Function sig) -> let combinations = allChoices (replicate n types) in
                            nub $ concatMap (fromMaybe [] . sig) combinations
                        _ -> []

        classes :: TypeCheckerMonad [(Int, Name)]
        classes = do
            st <- gets store
            let cls = filter (\id -> head id /= '.') (Map.keys st)
            let pairs = nub (map classNumberAndName cls)
            return pairs

        classNumberAndName :: String -> (Int, String)
        classNumberAndName str = go str "" False (0, "")
            where
                go :: String -> String -> Bool -> (Int, String) -> (Int, String)
                go str acc flag (num, name) = 
                    if head str == ' ' then
                        if flag == False then
                            go (tail str) "" True (stringToInt acc 0, name)
                        else
                            (num, acc)
                    else
                        go (tail str) (acc ++ [head str]) flag (num, name)
        
        stringToInt :: String -> Int -> Int
        stringToInt [] acc = acc
        stringToInt (c : rest) acc = stringToInt rest (acc * 10 + ord c - 48)

nameOfArg :: AST.ParameterSpan -> Name
nameOfArg (AST.Param (AST.Ident name _) _ _ annot) = name

fromMaybeResult :: Maybe Result -> Result
fromMaybeResult = fromMaybe []

getEnvs :: TypeCheckerMonad [Env]
getEnvs = do
    runs <- gets runs
    let (envs, _) = unzip runs
    return envs

getInfers :: TypeCheckerMonad [Inferred]
getInfers = do
    runs <- gets runs
    let (_, infers) = unzip runs
    return infers

-- update function inferred list
newFinf :: FInferred -> Name -> SL.SrcSpan -> [Type] -> [Type] -> FInferred
newFinf [] name annot args res = [(name, annot, [(args, res)])]
newFinf (finf@(name', annot', apps') : rest) name annot args res =
    if name' == name && annot' == annot then
        (name, annot, (args, res) : apps') : rest
    else
        finf : newFinf rest name annot args res

fromRetState :: RetState -> [Type]
fromRetState NoRet = []
fromRetState (RetDone t) = nub $ t
fromRetState (RetTBD t) = nub $ (t)

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a
snd3 :: (a, b, c) -> b
snd3 (_, b, _) = b
trd3 :: (a, b, c) -> c
trd3 (_, _, c) = c

getIndexType :: Type -> Type
getIndexType (List _) = int
getIndexType (Tuple _) = int
getIndexType AnyTuple = int

getContentFromIndexable :: Type -> Env -> TypeCheckerMonad [Type]
getContentFromIndexable (List Nothing) _ = return []
getContentFromIndexable (List (Just t)) _ = return [t]
getContentFromIndexable (Tuple t) _ = return t
getContentFromIndexable AnyTuple env = domain env
getContentFromIndexable _ _ = return []

-- merge the results of parallel branches
mergeSplits :: [([Run], RetState)] -> ([Run], RetState)
mergeSplits splits = if all (\(a, b) -> fromRet b == "RetDone") splits then
        ([], RetDone (nub $ concat (map (fromRetState . snd) splits)))
    else
        merge' splits
    where
        fromRet :: RetState -> String
        fromRet NoRet = "NoRet"
        fromRet (RetTBD _) = "RetTBD"
        fromRet (RetDone _) = "RetDone"

        merge' :: [([Run], RetState)] -> ([Run], RetState)
        merge' [] = ([], NoRet)
        merge' ((runs, ret) : rest) = let (resRuns, resRet) = merge' rest in
            case ret of
                NoRet -> (nub $ runs ++ resRuns, resRet)
                RetTBD types -> (nub $ runs ++ resRuns, RetTBD (nub $ types ++ fromRetState resRet))
                RetDone types -> (resRuns, RetTBD (nub $ types ++ fromRetState resRet))

isIndexable :: Type -> Bool
isIndexable (List _) = True
isIndexable (Tuple _) = True
isIndexable AnyTuple = True
isIndexable _ = False

allParameterCombinations :: Int -> Env -> TypeCheckerMonad [[Type]]
allParameterCombinations n env = do
    dom <- domain env
    return $ replicate n dom

-- starting env, starting functions env, name to types lists -> new starting environment
funcInitialEnv :: Env -> FEnv -> [Name] -> [Type] -> Env
funcInitialEnv env fenv names types = 
    updateEnv names types (updateWithFenv env fenv)
    where
        updateEnv :: [Name] -> [Type] -> Env -> Env
        updateEnv [] [] acc = acc
        updateEnv (name : rn) (typ : rt) acc = updateEnv rn rt (Map.insert name typ acc)

        updateWithFenv :: Env -> FEnv -> Env
        updateWithFenv env fenv = 
            let assocs = Map.assocs fenv in
                let (names, vals) = unzip assocs in
                    let (sigs, _) = unzip vals in
                        updateEnv names (map Function sigs) env

fenvToEnv :: FEnv -> Env
fenvToEnv fenv = 
    let assocs = Map.assocs fenv in
        let (keys, elems) = unzip assocs in
            let (sigs, params) = unzip elems in
                Map.fromList (zip keys (map Function sigs))

argsOfFunction :: AST.StatementSpan -> [AST.ParameterSpan]
argsOfFunction (AST.Fun (AST.Ident _ _) args _ _ _) = args

classConstructorNum :: Type -> Int
classConstructorNum (Class _ (_, n)) = n
classConstructorNum _ = -1