module TypeChecker where

import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List
import Data.Maybe

import Types
import Utils
import Unification
import Signatures
import qualified Store as Store

import qualified Language.Python.Common.AST as AST
import qualified Language.Python.Common.Token as Token 
import qualified Language.Python.Common.SrcLocation as SL

-- typechecking
-- statements
checkStmt :: [AST.StatementSpan] -> RetState -> TypeCheckerMonad RetState
checkStmt [] ret = return ret
checkStmt (AST.Import items annot : rest) ret = undefined
checkStmt (AST.FromImport _module items annot : rest) ret = undefined
checkStmt (AST.While cond body _else annot : rest) ret = do
    runs <- gets runs
    splits <- mapM (checkWhileForRun cond body _else ret) runs
    
    let (newRuns, newRet) = mergeSplits splits

    modify (\st -> st { runs = newRuns })

    checkStmt rest newRet

    where
        checkWhileForRun :: AST.ExprSpan -> AST.SuiteSpan -> AST.SuiteSpan -> RetState -> Run -> TypeCheckerMonad ([Run], RetState)
        checkWhileForRun cond body _else ret run = do
            typ <- checkExpr cond run
            if typ == [] then
                return ([], RetDone [])
            else do
                splits <- mapM (checkBranch ret run) [(body ++ _else), body, _else] 
                return $ mergeSplits splits
checkStmt (AST.For targets generator body _else annot : rest) ret = do
    runs <- gets runs
    case generator of
        (AST.List [] _) -> do
            splits <- mapM (\run -> checkBranch ret run _else) runs
            let (newRuns, newRet) = mergeSplits splits
            modify (\st -> st { runs = newRuns })
            checkStmt rest newRet
        (AST.Tuple [] _) -> do
            splits <- mapM (\run -> checkBranch ret run _else) runs
            let (newRuns, newRet) = mergeSplits splits
            modify (\st -> st { runs = newRuns })
            checkStmt rest newRet
        _ -> do
            splits <- mapM (checkForForRun targets generator body _else ret) runs

            let (newRuns, newRet) = mergeSplits splits

            modify (\st -> st { runs = newRuns })

            checkStmt rest newRet

    where
        checkForForRun :: [AST.ExprSpan] -> AST.ExprSpan -> AST.SuiteSpan -> AST.SuiteSpan -> RetState -> Run -> TypeCheckerMonad ([Run], RetState)
        checkForForRun targets generator body _else ret run = do
            genTypes <- checkExpr generator run
            newRuns <- mapM (assignFromGenerator targets run) genTypes 
            splits <- mapM (checkBranchesForRun body _else ret) (concat newRuns)
            return $ mergeSplits splits

        assignFromGenerator :: [AST.ExprSpan] -> Run -> Type -> TypeCheckerMonad [Run]
        assignFromGenerator targets run@(env, _) genType = do
            if isIndexable genType == False then
                return []
            else do
                itTypes <- getContentFromIndexable genType env
                if length targets == 1 then do
                    newRuns <- mapM (assignWithType (head targets) run) itTypes
                    return $ concat newRuns
                else do
                    newRuns <- mapM (assignWithType (AST.Tuple targets annot) run) itTypes
                    return $ concat newRuns

        checkBranchesForRun :: AST.SuiteSpan -> AST.SuiteSpan -> RetState -> Run -> TypeCheckerMonad ([Run], RetState)
        checkBranchesForRun body _else ret run = do
            splits <- mapM (checkBranch ret run) [(body ++ _else), body]
            return $ mergeSplits splits

checkStmt (AST.AsyncFor stnt annot : rest) ret = undefined
checkStmt (fun@(AST.Fun (AST.Ident ident _) args _ body annot) : rest) ret = checkStmt rest ret
checkStmt (AST.AsyncFun def annot : rest) ret = undefined
checkStmt (AST.Class name args body annot : rest) ret = checkStmt rest ret
checkStmt (AST.Conditional guards _else annot : rest) ret = do
    runs <- gets runs
    let extraGuard = if _else == [] then [((AST.Bool True blankAnnot), [])] else [((AST.Bool True blankAnnot), _else)]
    splits <- mapM (handleAllIfs (guards ++ extraGuard) ret annot) runs
    
    let (newRuns, newRet) = mergeSplits splits

    modify (\st -> st { runs = newRuns })

    checkStmt rest newRet

    where
        handleAllIfs :: [(AST.ExprSpan, AST.SuiteSpan)] -> RetState -> SL.SrcSpan -> Run -> TypeCheckerMonad ([Run], RetState)
        handleAllIfs guards ret annot run = do
            splits <- mapM (checkGuardAndExecute ret annot run) guards
            return $ mergeSplits splits

        -- for a retstate, annot and run, check the guard and if its correct, return all run continuations through branch
        checkGuardAndExecute :: RetState -> SL.SrcSpan -> Run -> (AST.ExprSpan, AST.SuiteSpan) -> TypeCheckerMonad ([Run], RetState)
        checkGuardAndExecute ret annot run (guard, branch) = do
            typ <- checkExpr guard run
            if typ == [] then
                return $ ([], RetDone [])
            else do
                modify (\st -> st { runs = [run] })
                retst <- checkStmt branch ret
                newRuns <- gets runs
                return (newRuns, retst)
checkStmt (AST.Assign [to] expr annot : rest) ret = do 
    runs <- gets runs
    newRuns <- mapM (assignExpr to expr) runs
    modify (\st -> st { runs = concat newRuns })

    checkStmt rest ret
    where
        assignExpr :: AST.ExprSpan -> AST.ExprSpan -> Run -> TypeCheckerMonad [Run]
        assignExpr (AST.Tuple [] _) (AST.Tuple [] _) run = return []
        assignExpr (AST.Tuple vars@(var : rv) annot1) (AST.Tuple exprs@(expr : re) annot2) run = do
            if length vars /= length exprs then
                return []
            else do
                typesList <- checkExprL exprs run
                let combs = allChoices typesList
                newRuns <- mapM (assignMultiple vars run) combs
                return $ concat newRuns
        assignExpr var expr run = do
            assignExprWithOneRun var expr run

        assignExprWithOneRun :: AST.ExprSpan -> AST.ExprSpan -> Run -> TypeCheckerMonad [Run]
        assignExprWithOneRun var expr run = do
            exprTypes <- checkExpr expr run
            newRuns <- mapM (assignWithType var run) exprTypes
            return $ concat newRuns    
checkStmt (AST.AugmentedAssign to op expr annot : rest) ret = undefined
checkStmt (AST.AnnotatedAssign assign_annot to expr annot : rest) ret = undefined
checkStmt (AST.Decorated decorators def annot : rest) ret = undefined
checkStmt (AST.Return expr annot : rest) ret = do
    inFun <- gets inFunction
    case inFun of
        False -> formatError annot "\'return\' outside function"
        True ->
            case expr of
                Nothing -> do
                    runs <- gets runs
                    if runs == [] then
                        checkStmt [] $ updateReturn ret []
                    else
                        checkStmt [] $ updateReturn ret [_void]
                Just exp -> do
                    runs <- gets runs
                    let (envs, infers) = unzip runs
                    types <- mapM (checkExpr exp) runs
                    let newRet = updateReturn ret (concat types)
                    checkStmt [] newRet
checkStmt (AST.Try body excepts _else finally annot : rest) ret = undefined
checkStmt (AST.Raise expr annot : rest) ret = undefined
checkStmt (AST.With context body annot : rest) ret = undefined
checkStmt (AST.AsyncWith stmt annot : rest) ret = undefined
checkStmt (AST.Pass annot : rest) ret = undefined
checkStmt (AST.Break annot : rest) ret = do
    return ret
checkStmt (AST.Continue annot : rest) ret = do
    return ret
checkStmt (AST.Delete exprs annot : rest) ret = undefined
checkStmt (AST.StmtExpr expr annot : rest) ret = do
    runs <- gets runs
    types <- mapM (checkExpr expr) runs
    let pairs = zip runs types
    let newRuns = concatMap (\(run, types) -> if types == [] then [] else [run]) pairs
    modify (\st -> st {runs = newRuns})
    checkStmt rest ret
checkStmt (AST.Global vars annot : rest) ret = undefined
checkStmt (AST.NonLocal vars annot : rest) ret = undefined
checkStmt (AST.Assert exprs annot : rest) ret = undefined
checkStmt (AST.Print chevron exprs trailing_comma annot : rest) ret = undefined
checkStmt (AST.Exec expr globals_locals annot : rest) ret = undefined
checkStmt stmt ret = do
    formatError (SL.SpanEmpty) $ "This should not happen: " ++ show stmt

-- assignment functions
-- assign type/types to var/vars
assignWithType :: AST.ExprSpan -> Run -> Type -> TypeCheckerMonad [Run]
assignWithType (AST.Var (AST.Ident ident _) annot) run@(env, inferred) typ = return [(Map.insert ident typ env, inferred ++ [(ident, annot, typ)])]
assignWithType dot@(AST.Dot expr (AST.Ident ident2 annot1) annot2) run typ = do
    case expr of
        (AST.Var (AST.Ident ident1 annot3) annot4) -> do
            fA <- gets firstArg
            case fA of
                Nothing -> checkNormalAssignment dot run typ
                Just (name, t) -> 
                    if name == ident1 then
                        assignWithType (AST.Var (AST.Ident (ident1 ++ "." ++ ident2) annot3) annot4) run typ
                    else
                        checkNormalAssignment dot run typ
        _ -> checkNormalAssignment dot run typ
    where
        checkNormalAssignment :: AST.ExprSpan -> Run -> Type -> TypeCheckerMonad [Run]
        checkNormalAssignment dot run typ = do
            dotType <- checkExpr dot run
            if typ `elem` dotType then
                return [run]
            else
                return []
assignWithType (AST.Tuple vars annot) run@(env, _) AnyTuple = do
    combs <- allParameterCombinations (length vars) env
    let combinations = allChoices combs
    newRuns <- mapM (assignMultiple vars run) combinations
    return $ concat newRuns
assignWithType (AST.Tuple vars annot) run (Tuple types) = do
    newRuns <- assignMultiple vars run types
    return newRuns
assignWithType _ _ _ = return []

assignMultiple :: [AST.ExprSpan] -> Run -> [Type] -> TypeCheckerMonad [Run]
assignMultiple vars run types = do
    results <- go vars [run] types
    return results
    where
        go :: [AST.ExprSpan] -> [Run] -> [Type] -> TypeCheckerMonad [Run]
        go [] runs [] = return runs
        go (var : rv) runs (typ : rt) = do
            newRuns <- mapM (\run -> assignWithType var run typ) runs
            go rv (concat newRuns) rt
        go _ _ _ = return []

-- for a return state and run, computes given statements and returns new state of runs and return
checkBranch :: RetState -> Run -> AST.SuiteSpan -> TypeCheckerMonad ([Run], RetState)
checkBranch ret run branch = do
    modify (\st -> st { runs = [run] })
    retst <- checkStmt branch ret
    newRuns <- gets runs
    return (newRuns, retst)

-- expressions
checkExprL :: [AST.ExprSpan] -> Run -> TypeCheckerMonad [[Type]]
checkExprL [] _ = return []
checkExprL (expr : rest) run = do
    exprH <- checkExpr expr run
    exprL <- checkExprL rest run
    return $ exprH : exprL

checkExpr :: AST.ExprSpan -> Run -> TypeCheckerMonad [Type]
checkExpr (AST.Var (AST.Ident ident _) annot) (env, _) = do
    case Map.lookup ident env of
        Nothing -> do
            inFun <- gets inFunction
            if inFun == False then
                return []
            else do
                modify (\st -> st { newParameters = ident : newParameters st })
                domain env
        Just typ -> return [typ]
checkExpr (AST.Int value lit annot) _ = return $ [int]
checkExpr (AST.LongInt value lit annot) _ = return $ [int]
checkExpr (AST.Float value lit annot) _ = return $ [float]
checkExpr (AST.Imaginary value lit annot) _ = return $ [complex]
checkExpr (AST.Bool value annot) _ = return $ [bool]
checkExpr (AST.None annot) (env, _) = undefined
checkExpr (AST.Ellipsis annot) (env, _) = undefined
checkExpr (AST.ByteStrings strings annot) (env, _) = undefined
checkExpr (AST.Strings strings annot) _ = return $ [str]
checkExpr (AST.UnicodeStrings strings annot) (env, _) = undefined
-- works only for AST.ArgumentSpan ArgExpr
checkExpr (AST.Call fun args annot) run@(env, _) = do
    let funname = getFunName fun
    if funname == "print" then
        apply' Nothing (Function printSig) []
    else if funname == "type" then
        apply' Nothing (Function typeSig) []
    else if funname == "range" then
        apply' Nothing (Function rangeSig) []
    else do
        sig' <- checkExpr fun run
        let sig = if null sig' then Function nothing else head sig' -- there should be only 1 anyways
        fenv <- gets functions
        let extraParams = getParams funname fenv
        farg <- getTypeUntilLastDot fun
        if '.' `elem` funname then
            apply' farg sig []
        else
            case Map.lookup funname env of
                Nothing -> do
                        return []
                Just func -> do
                    apply' farg sig extraParams
    where
        apply' :: Maybe Type -> Type -> ExtraParams -> TypeCheckerMonad [Type]
        apply' farg func extraParams = do
            let exprs = map unpackExpression (args ++ extraParams)
            types <- checkExprL exprs run
            let prefix = checkFirstArg farg
            let types' = replaceListsAndTuples types
            let combinations = allChoices types'
            let results = nub $ concat $ map fromMaybeResult $ map (toFunction func) (map (prefix ++) combinations)
            return results
            where
                checkFirstArg :: Maybe Type -> [Type]
                checkFirstArg Nothing = []
                checkFirstArg (Just t) = [t]

                replaceListsAndTuples :: [[Type]] -> [[Type]]
                replaceListsAndTuples = map replaceWithAny
                    where
                        replaceWithAny :: [Type] -> [Type]
                        replaceWithAny = map replace
                            where
                                replace :: Type -> Type
                                replace (Tuple _) = AnyTuple
                                replace t = t

        getTypeUntilLastDot :: AST.ExprSpan -> TypeCheckerMonad (Maybe Type)
        getTypeUntilLastDot (AST.Var _ _) = return Nothing
        getTypeUntilLastDot (AST.Dot expr _ _) = do
            typ <- checkExpr expr run
            if null typ then
                formatError annot "Expression before last dot has to have a type"
            else
                return $ Just (head typ)

        getParams :: Name -> FEnv -> ExtraParams
        getParams name fenv = 
            case Map.lookup name fenv of
                Nothing -> []
                Just (_, res) -> res

        unpackExpression :: AST.ArgumentSpan -> AST.ExprSpan
        unpackExpression (AST.ArgExpr expr _) = expr

        getFunName :: AST.ExprSpan -> Name
        getFunName (AST.Var (AST.Ident ident _) annot) = ident
        getFunName (AST.Dot expr (AST.Ident ident _) annot) = getFunName expr ++ "." ++ ident
        getFunName _ = ""

        toFunction :: Type -> Signature
        toFunction (Function sig) = sig
        toFunction _ = nothing
checkExpr (AST.Subscript subscriptee expr annot) run@(env, _) = do
    subs <- checkExpr subscriptee run
    index <- checkExpr expr run
    results <- mapM (getValue index) subs 
    return $ nub (concat results)
    where
        getValue :: [Type] -> Type -> TypeCheckerMonad [Type]
        getValue index typ =
            if isIndexable typ then
                if getIndexType typ `elem` index then
                    getContentFromIndexable typ env
                else
                    return []
            else
                return []
checkExpr (AST.SlicedExpr slicee slices annot) run = do
    slic <- checkExpr slicee run
    index <- mapM (checkSlice run) slices
    if all (all (\l -> null l || any (int ==) l)) index then do
        return $ (nub (filter (\typ -> isIndexable typ && typ /= List Nothing) slic))
    else do
        return []
    where
        checkSlice :: Run -> AST.SliceSpan -> TypeCheckerMonad [[Type]]
        checkSlice run (AST.SliceProper lower upper stride annot) = do
            lt <- check run lower
            ut <- check run upper
            st <- doublecheck run stride
            return [lt, ut, st]
            where
                check :: Run -> Maybe AST.ExprSpan -> TypeCheckerMonad [Type]
                check run e = case e of
                    Nothing -> return []
                    Just expr -> do 
                        res <- checkExpr expr run
                        case res of
                            [] -> return [_void]
                            t -> return t

                doublecheck :: Run -> Maybe (Maybe AST.ExprSpan) -> TypeCheckerMonad [Type]
                doublecheck run e1 = case e1 of
                    Nothing -> return []
                    Just e2 -> case e2 of
                        Nothing -> return []
                        Just expr -> do 
                            res <- checkExpr expr run
                            case res of
                                [] -> return [_void]
                                t -> return t           
checkExpr (AST.CondExpr true_branch cond false_branch annot) (env, _) = undefined
checkExpr (AST.BinaryOp operator larg rarg annot) run = do
    func <- getFunction operator
    typl <- checkExpr larg run
    typr <- checkExpr rarg run
    let combinations = allChoices [typl, typr]
    let results = map func combinations
    return (nub $ foldr (\res acc -> fromMaybeResult res ++ acc) [] results)
checkExpr (AST.UnaryOp operator arg annot) run = do
    func <- getFunction operator
    typ <- checkExpr arg run
    let combinations = allChoices [typ]
    let results = map func combinations
    return (nub $ foldr (\res acc -> fromMaybeResult res ++ acc) [] results)
checkExpr (AST.Dot expr (AST.Ident ident annot1) annot2) run = do
    baseTypes <- checkExpr expr run
    if baseTypes /= [] then do
        attrsAndMethods <- mapM (findMethodOrType ident) baseTypes
        return $ (nub . concat) attrsAndMethods
    else do
        fA <- gets firstArg
        case (expr, fA) of
            ((AST.Var (AST.Ident ident2 _) _), Just (name, typ)) ->
                if name == ident2 then checkExpr (AST.Var (AST.Ident (ident2 ++ "." ++ ident) annot1) annot2) run else return []
            _ -> return []
    where
        handlePrefix :: AST.ExprSpan -> Run -> TypeCheckerMonad [Type]
        handlePrefix expr run = do
            typ <- checkExpr expr run
            if null typ then do
                fA <- gets firstArg
                case (expr, fA) of
                    ((AST.Var (AST.Ident ident _) _), Just (name, typ)) ->
                        if name == ident then return [typ] else return []
                    _ -> return []
            else
                return typ

        findMethodOrType :: Name -> Type -> TypeCheckerMonad [Type]
        findMethodOrType ident typ = do
            st <- gets store
            case Map.lookup (Store.typeIdentifier typ) st of
                Nothing -> return []
                Just (attrs, methods) -> return $ (
                    case Map.lookup ident attrs of
                        Nothing -> []
                        Just typ -> [typ]) ++ (
                    case Map.lookup ident methods of
                        Nothing -> []
                        Just sig -> [sig])
checkExpr (AST.Lambda args body annot) env = undefined
checkExpr (AST.Tuple exprs annot) run = do
    types <- checkExprL exprs run
    let combinations = allChoices types
    let finalCombinations = filter (\types -> length types /= 0) combinations
    return $ map Tuple combinations
checkExpr (AST.Yield arg annot) env = undefined
checkExpr (AST.Generator comprehension annot) env = undefined
checkExpr (AST.Await expr annot) env = undefined
checkExpr (AST.ListComp comprehension annot) env = undefined
checkExpr (AST.List exprs annot) run = do
    types <- checkExprL exprs run
    if null types then
        return [List Nothing]
    else if length (head types) == 1 && all (\list -> list == head types) types then
        return [List (Just $ head $ head types)]
    else
        return []
checkExpr (AST.Dictionary mappings annot) env = undefined
checkExpr (AST.DictComp comprehension annot) env = undefined
checkExpr (AST.Set exprs annot) env = undefined
checkExpr (AST.SetComp comprehension annot) env = undefined
checkExpr (AST.Starred expr annot) env = undefined
checkExpr (AST.Paren expr annot) run = checkExpr expr run
checkExpr (AST.StringConversion expr annot) env = undefined

typecheckAllFunctions :: [AST.StatementSpan] -> TypeCheckerMonad (FEnv, FInferred)
typecheckAllFunctions funcs' = do
    let funcs = map addReturn funcs'
    llStarts <- mapM adHocFRuns funcs
    let listOfFRuns = concat llStarts
    let n = length listOfFRuns
    functions <- typeCheckInCyclicQueue listOfFRuns Map.empty (n + 1) (n + 1)
    return $ preprocessFunctions functions

    where
        addReturn :: AST.StatementSpan -> AST.StatementSpan
        addReturn (AST.Fun (AST.Ident ident a1) args resan body a2) = (AST.Fun (AST.Ident ident a1) args resan (body ++ [AST.Return Nothing blankAnnot]) a2)

        preprocessFunctions :: [(AST.StatementSpan, [Type], [Type])] -> (FEnv, FInferred)
        preprocessFunctions [] = (Map.empty, [])
        preprocessFunctions ((AST.Fun (AST.Ident ident _) args _ body annot, types, results) : rest) = 
            if results == [] then
                preprocessFunctions rest
            else
                let (fenv, finf) = preprocessFunctions rest in 
                    (newFenv fenv ident types results, newFinf finf ident annot types results)
            where
                newFenv :: FEnv -> Name -> [Type] -> [Type] -> FEnv
                newFenv fenv name args res = 
                    case Map.lookup name fenv of
                        Nothing -> Map.insert name (updateSignature (args `onesig` res) nothing, []) fenv
                        Just (sig, param) -> Map.insert name (updateSignature (args `onesig` res) sig, param) fenv

                threesToTwos :: [(a, b, c)] -> [(a, b)]
                threesToTwos list = 
                    let (as, bs, cs) = unzip3 list in
                        zip as bs

        typeCheckInCyclicQueue :: [(AST.StatementSpan, [Type], [Type])] -> FEnv -> Int -> Int -> TypeCheckerMonad [(AST.StatementSpan, [Type], [Type])]
        typeCheckInCyclicQueue [] _ _ _ = return []
        typeCheckInCyclicQueue list _ _ 0 = return list
        typeCheckInCyclicQueue ((fun@(AST.Fun (AST.Ident ident _) args _ body annot), types, results) : rest) fenv n m = do
            -- assumption: there is only one run to take environment from
            st <- gets store
            ops <- gets operators
            runs <- gets runs
            let env = fst (head runs)
            case runTypeCheckerMonad (initTCState Nothing True st ops [(funcInitialEnv env fenv (map nameOfArg args) types, [])] fenv [] []) (checkStmt body NoRet) of
                Left err -> formatError annot err
                Right (retst, tcstate) -> let retList = fromRetState retst in
                    if length retList > length results then do
                        typeCheckInCyclicQueue (rest ++ [(fun, types, retList)]) (updateFEnv fenv ident types retList) n n
                    else do
                        typeCheckInCyclicQueue (rest ++ [(fun, types, results)]) fenv n (m - 1)
            where
                updateFEnv :: FEnv -> Name -> [Type] -> [Type] -> FEnv
                updateFEnv fenv name arg res = 
                    case Map.lookup name fenv of
                        Nothing -> Map.insert name (updateSignature (arg `onesig` res) nothing, []) fenv
                        Just (sig, param) -> Map.insert name (updateSignature (arg `onesig` res) sig, param) fenv

        -- returning (function, argument types, return types)
        adHocFRuns :: AST.StatementSpan -> TypeCheckerMonad [(AST.StatementSpan, [Type], [Type])]
        adHocFRuns fun@(AST.Fun (AST.Ident ident _) args _ body annot) = do
            runs <- gets runs
            let (env, _) = head runs
            combs <- allParameterCombinations (length args) env
            let combinations = allChoices combs
            return $ map (\args -> (fun, args, [])) combinations

typecheckClasses :: [AST.StatementSpan] -> Store -> Operators -> [CInferred] -> Env -> ClassGraph -> TypeCheckerMonad (Store, Operators, [CInferred], Env)
typecheckClasses [] st ops cinf env _ = return (st, ops, cinf, env)
typecheckClasses (cl : rest) st ops cinf env graph = do
    (newStore, newOps, newInf, newEnv, newGraph) <- typecheckClass cl st ops env graph
    typecheckClasses rest newStore newOps (cinf ++ [newInf]) (Map.union newEnv env) newGraph

    where
        typecheckClass :: AST.StatementSpan -> Store -> Operators -> Env -> ClassGraph -> TypeCheckerMonad (Store, Operators, CInferred, Env, ClassGraph)
        typecheckClass (AST.Class (AST.Ident ident _) args body annot) store ops env graph = do
            let (init, add, otherMethods) = extractImportantMethods "__init__" "__add__" body
            let parentName = if null args then "" else (\(AST.ArgExpr (AST.Var (AST.Ident ident _) _) _) -> ident) (head args)
            let newGraph = Map.insert ident (parentName, init, add, otherMethods) graph
            (finalInit, finalAdd, finalMethods) <- getFinalBodies init add otherMethods parentName newGraph
            
            let initArgs = if finalInit == Nothing then [] else map nameOfArg (argsOfFunction (fromJust finalInit))
            if null initArgs && finalInit /= Nothing then
                return (st, ops, (ident, annot, []), env, graph)
            else do
                combs <- allParameterCombinations (length initArgs - 1) env
                let combinations = map (\args -> (Class ident (args, 0)) : args) (allChoices combs)

                initPacks <- mapM (typecheckInit finalInit store ops env) combinations
                let (newStore, newCombs) = modifyWithInitPacks (concat initPacks) ident 1 (store, [])
                let finalEnv = updateEnvWithClass newCombs env
                classPacks <- typecheckClasses newStore ops finalEnv finalAdd finalMethods newCombs
                let result = handleClassPacks ident classPacks store ops finalEnv (ident, annot, []) newGraph
                return result 
            where
                updateEnvWithClass :: [(Argument, Inferred)] -> Env -> Env
                updateEnvWithClass [] env = env
                updateEnvWithClass ((args, _) : rest) env = updateEnvWithClass rest (let args' = tail args in
                    case Map.lookup ident env of
                        Nothing -> Map.insert ident (Function (args' `onesig` [head args])) env
                        Just (Function sig) -> Map.insert ident (Function (updateSignature (args' `onesig` (fromMaybe [] (sig args') ++ [head args])) sig)) env)

                modifyWithInitPacks :: [(Argument, Env, Inferred)] -> Name -> Int -> (Store, [(Argument, Inferred)]) -> (Store, [(Argument, Inferred)])
                modifyWithInitPacks [] ident _ (st, arg) = (st, arg)
                modifyWithInitPacks ((args, env, inf) : rest) ident n (st, arg) = modifyWithInitPacks rest ident (n + 1) (
                    Map.insert (Store.typeIdentifier $ Class ident (tail args, n)) (env, Map.empty) st,
                    arg ++ [(((\(Class id (ts, _)) -> (Class id (ts, n))) (head args) : tail args), inf)])

                typecheckInit :: Maybe AST.StatementSpan -> Store -> Operators -> Env -> [Type] -> TypeCheckerMonad [(Argument, Env, Inferred)]
                typecheckInit Nothing store ops env argTypes = return [(argTypes, Map.empty, [])]
                typecheckInit (Just (AST.Fun (AST.Ident _ _) args _ body _)) store ops env argTypes = do
                    if null args then
                        return []
                    else do
                        attrPack <- tryInit (nameOfArg (head args), Class ident (tail argTypes, 0)) store ops env body (map nameOfArg args) argTypes
                        return attrPack
                    where
                        tryInit :: (Name, Type) -> Store -> Operators -> Env -> [AST.StatementSpan] -> [Name] -> [Type] -> TypeCheckerMonad [(Argument, Env, Inferred)] -- return parameters and attributes
                        tryInit self@(name, _) store ops env body names types = do
                            case runTypeCheckerMonad (initTCState (Just self) False store ops [(funcInitialEnv env Map.empty names types, [])] Map.empty [] []) (checkStmt body NoRet) of
                                Right (NoRet, tcstate) -> do
                                    let finalRuns = runs tcstate
                                    if null finalRuns then
                                        return []
                                    else do
                                        let attrsNames = nub $ map fst3 (filter (\inf@(ident, _, _) -> take (length name + 1) ident == (name ++ ".")) ((concat . snd . unzip) finalRuns))
                                        let packs = map (getAttributes (length name + 1) attrsNames) ((snd . unzip) finalRuns)
                                        return $ map (\(a, b) -> (types, a, b)) packs
                                _ -> return []
                            where
                                getAttributes :: Int -> [Name] -> Inferred -> (Env, Inferred)
                                getAttributes n names inf = foldr (handleWithLast n inf) (Map.empty, []) names

                                handleWithLast :: Int -> Inferred -> Name -> (Env, Inferred) -> (Env, Inferred)
                                handleWithLast n inferred name acc@(env, inf) = 
                                    case findLast name inferred of
                                        Nothing -> acc
                                        Just last@(_, annot, typ) -> (Map.insert (drop n name) typ env, inf ++ [last])

                                findLast :: Name -> Inferred -> Maybe (Name, SL.SrcSpan, Type)
                                findLast _ [] = Nothing
                                findLast name (inf@(ident, _, _) : rest) = 
                                    case findLast name rest of
                                        Just res -> Just res
                                        Nothing -> if name == ident then Just inf else Nothing

                typecheckClasses :: Store -> Operators -> Env -> Maybe AST.StatementSpan -> [AST.StatementSpan] -> [(Argument, Inferred)] -> TypeCheckerMonad [(Argument, Store, Operators, Inferred, FInferred)]
                typecheckClasses store ops env add other argsList = do
                    let packs = map (\(args, inf) -> (args, store, ops, inf, [])) argsList
                    newPacks <- typecheckClassesInACyclicQueue store ops env add other (length packs + 1) (length packs + 1) packs
                    return newPacks
                    where
                        typecheckClassesInACyclicQueue :: Store -> Operators -> Env -> Maybe AST.StatementSpan -> [AST.StatementSpan] -> Int -> Int -> [(Argument, Store, Operators, Inferred, FInferred)] -> TypeCheckerMonad [(Argument, Store, Operators, Inferred, FInferred)]
                        typecheckClassesInACyclicQueue _ _ _ _ _ _ _ [] = return []
                        typecheckClassesInACyclicQueue store ops env _ _ _ 0 packs = return $ map (\(a, s, o, i, f) -> (a, store, ops, i, f)) packs
                        typecheckClassesInACyclicQueue store ops env add other n m ((args, _, _, inf, finf) : rp) = do
                            newPack <- typecheckClassWithArguments store ops env add other args
                            case newPack of
                                Nothing -> typecheckClassesInACyclicQueue store ops env add other n (m - 1) rp
                                Just (types, newStore, newOps, newFinf) -> do
                                    let pack = (types, newStore, newOps, inf, newFinf)
                                    if finf == newFinf then
                                        typecheckClassesInACyclicQueue newStore newOps env add other n (m - 1) (rp ++ [pack])
                                    else do
                                        typecheckClassesInACyclicQueue newStore newOps env add other n n (rp ++ [pack])


                typecheckClassWithArguments :: Store -> Operators -> Env -> Maybe AST.StatementSpan -> [AST.StatementSpan] -> Argument -> TypeCheckerMonad (Maybe (Argument, Store, Operators, FInferred)) -- constructor argument, attributes, methods, operator overloads
                typecheckClassWithArguments store ops env add other args = do
                    modify (\st -> st { store = store, operators = ops })
                    let self = head args
                    (finalOps, finfAdd) <- typecheckAdd add store ops env self
                    (finalStore, finf) <- typecheckMethods other store finalOps env self finfAdd
                    return $ Just (args, finalStore, finalOps, finf)
                    where
                        typecheckMethods :: [AST.StatementSpan] -> Store -> Operators -> Env -> Type -> FInferred -> TypeCheckerMonad (Store, FInferred)
                        typecheckMethods [] store _ _ _ finf = return (store, finf)
                        typecheckMethods ((AST.Fun (AST.Ident name _) args _ body annot) : rest) store ops env self finferred = do
                            modify (\st -> st {store = store})
                            combs <- allParameterCombinations (length args - 1) env
                            let combinations = map (self :) (allChoices combs)
                            let packs' = map (tryBlock True store ops env body (map nameOfArg args)) combinations
                            let packs = catMaybes packs'
                            let (sig, finf) = handlePacks packs name annot (nothing, [])
                            typecheckMethods rest (newStore sig) ops env self (finferred ++ finf)
                            where
                                newStore :: Signature -> Store
                                newStore sig = case Map.lookup (Store.typeIdentifier self) store of
                                    Just (aenv, env) -> Map.insert (Store.typeIdentifier self) (aenv, Map.insert name (Function sig) env) store

                        typecheckAdd :: Maybe AST.StatementSpan -> Store -> Operators -> Env -> Type -> TypeCheckerMonad (Operators, FInferred)
                        typecheckAdd Nothing store ops env self = return (ops, [])
                        typecheckAdd (Just (AST.Fun (AST.Ident name _) args _ body annot)) store ops env self = do
                            if length args /= 2 then
                                return (ops, [])
                            else do
                                modify (\st -> st {store = store})
                                combs <- allParameterCombinations 1 env
                                let combinations = map (self :) (allChoices combs)
                                let addPacks' = map (tryBlock False store ops env body (map nameOfArg args)) combinations
                                let addPacks = catMaybes addPacks'
                                let (sig, finf) = handlePacks addPacks name annot (nothing, [])
                                case Map.lookup "pls" ops of
                                    Just addSig -> return (Map.insert "pls" (updateSignature sig addSig) ops, finf)

                        tryBlock :: Bool -> Store -> Operators -> Env -> [AST.StatementSpan] -> [Name] -> [Type] -> Maybe (Argument, Result)
                        tryBlock b store ops env body names types =
                            case runTypeCheckerMonad (initTCState Nothing True store ops [(funcInitialEnv env Map.empty names types, [])] Map.empty [] []) (checkStmt body NoRet) of
                                Right (retst, tcstate) -> let retList = fromRetState retst in
                                    if null retList then
                                        Nothing
                                    else
                                        Just (types, retList)
                                _ -> Nothing
                        
                        handlePacks :: [(Argument, Result)] -> Name -> SL.SrcSpan -> (Signature, FInferred) -> (Signature, FInferred)
                        handlePacks [] _ _ acc = acc
                        handlePacks ((arg, res) : rest) name annot (sig, finf) = handlePacks rest name annot (updateSignature (arg `onesig` res) sig, newFinf finf name annot arg res)

                getFinalBodies :: Maybe AST.StatementSpan -> Maybe AST.StatementSpan -> [AST.StatementSpan] -> Name -> ClassGraph -> TypeCheckerMonad (Maybe AST.StatementSpan, Maybe AST.StatementSpan, [AST.StatementSpan])
                getFinalBodies init add otherMethods parentName graph = if parentName == "" then return (init, add, otherMethods) else do
                    case Map.lookup parentName graph of
                        Nothing -> throwError $ "Class " ++ parentName ++ " not defined"
                        Just (_, i, a, o) -> do
                            let finalInit = if init /= Nothing then init else i
                            let finalAdd = if add /= Nothing then add else a
                            let finalMethods = mergeByName otherMethods o
                            return (finalInit, finalAdd, finalMethods)
                    where
                        mergeByName :: [AST.StatementSpan] -> [AST.StatementSpan] -> [AST.StatementSpan]
                        mergeByName methods [] = methods
                        mergeByName methods (m : rest) = 
                            if nameOfFunction m `elem` map nameOfFunction methods then
                                mergeByName methods rest
                            else
                                mergeByName (m : methods) rest

                        nameOfFunction :: AST.StatementSpan -> Name
                        nameOfFunction (AST.Fun (AST.Ident ident _) _ _ _ _) = ident

                handleClassPacks :: Name -> [(Argument, Store, Operators, Inferred, FInferred)] -> Store -> Operators -> Env -> CInferred -> ClassGraph -> (Store, Operators, CInferred, Env, ClassGraph)
                handleClassPacks name [] store operators env cinf graph = (store, operators, cinf, env, graph)
                handleClassPacks name ((args', st, ops, inf, finf) : rest) store operators environment (_, _, infs) graph = handleClassPacks name rest newStore newOps environment cinferred graph
                    where
                        args = tail args'
                        newStore = Map.union st store
                        newOps = 
                            let assocs1 = Map.assocs operators in
                            let assocs2 = Map.assocs ops in
                                mergeOperators assocs1 assocs2 Map.empty
                        cinferred = (name, annot, infs ++ [((args, classConstructorNum (head args')), inf, finf)])
                        mergeOperators :: [(Name, Signature)] -> [(Name, Signature)] -> Operators -> Operators
                        mergeOperators [] [] ops = ops
                        mergeOperators o1@((n1, sig1) : r1) o2@((n2, sig2) : r2) ops =
                            if length o1 /= length o2 then
                                error $ show (map fst o1) ++ "\n" ++ show (map fst o2)
                            else
                                mergeOperators r1 r2 (Map.insert n1 (updateSignature sig1 sig2) ops)
                
                extractImportantMethods :: Name -> Name -> [AST.StatementSpan] -> (Maybe AST.StatementSpan, Maybe AST.StatementSpan, [AST.StatementSpan])
                extractImportantMethods _ _ [] = (Nothing, Nothing, [])
                extractImportantMethods func1 func2 (fun@(AST.Fun (AST.Ident ident _) args _ body annot) : rest) = 
                    let (f1, f2, m) = extractImportantMethods func1 func2 rest in
                        if ident == func1 then
                            (Just fun, f2, m)
                        else if ident == func2 then
                            (f1, Just fun, m)
                        else
                            (f1, f2, fun : m)
                extractImportantMethods func1 func2 (stmt : rest) = extractImportantMethods func1 func2 rest

initialState :: TCState
initialState = initTCState Nothing False Store.store Signatures.startingOperators [(Map.empty, [])] Map.empty [] [] where

typecheck :: [AST.StatementSpan] -> IO (Either String String)
typecheck stmts = do
    let classes = collectClasses stmts
    let funcs = collectFunctions stmts
    case runTypeCheckerMonad initialState (typecheckClasses classes Store.store Signatures.startingOperators [] Map.empty Map.empty) of
        Left err -> return $ Left err
        Right ((st, ops, cinf, env), _) -> do
            case runTypeCheckerMonad (initTCState Nothing False st ops [(env, [])] Map.empty [] []) (typecheckAllFunctions funcs) of
                Left err -> return $ Left err
                Right ((fenv, finf), _) -> do
                    case runTypeCheckerMonad (initTCState Nothing False st ops [(Map.union (fenvToEnv fenv) env, [])] fenv finf []) (checkStmt stmts NoRet) of
                        Left err -> return $ Left err
                        Right (_, tcstate) -> do
                            return $ Right (prettyInferenceString cinf (fInferred tcstate) (runs tcstate))
    where
        newEnv = Map.empty

        collectClasses :: [AST.StatementSpan] -> [AST.StatementSpan]
        collectClasses [] = []
        collectClasses (cl@(AST.Class name args body annot) : rest) = cl : collectClasses rest
        collectClasses (_ : rest) = collectClasses rest

        collectFunctions :: [AST.StatementSpan] -> [AST.StatementSpan]
        collectFunctions [] = []
        collectFunctions (fun@(AST.Fun (AST.Ident ident _) args _ body annot) : rest) = fun : collectFunctions rest
        collectFunctions (_ : rest) = collectFunctions rest