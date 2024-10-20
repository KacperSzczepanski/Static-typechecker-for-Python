module Unification where

-- based on https://medium.com/@dhruvrajvanshi/type-inference-for-beginners-part-1-3e0a5be98a4b

import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as Map
import Data.List

import Types
import Utils
import Signatures

import qualified Language.Python.Common.AST as AST
import qualified Language.Python.Common.SrcLocation as SL

initContext :: Int -> UEnv -> Context
initContext n env = Context { next = n, env = env }
data Context = Context { next :: Int, env :: UEnv } deriving (Show)
type UnificationMonad a = StateT Context (Except String) a
runUnificationMonad :: Context -> UnificationMonad a -> Either String (a, Context)
runUnificationMonad st ev = runExcept (runStateT ev st)

testEnv :: UEnv
testEnv = foldr (\(name, typ) acc -> Map.insert name typ acc) Map.empty [
    ("-plus", UT (TFun (TNamed int) (TFun (TNamed int) (TNamed int)))),
    ("-minus", UT (TFun (TNamed int) (TFun (TNamed int) (TNamed int)))),
    ("-multiply", UT (TFun (TNamed int) (TFun (TNamed int) (TNamed int)))),
    ("-leq", FA (ForAll ["A"] (TFun (TVar "A") (TFun (TVar "A") (TNamed bool)))))]

data UType = TNamed Type
           | TVar Name
           | TFun UType UType
instance Show UType where
    show (TNamed t) = show t
    show (TVar name) = name
    show (TFun from to) = "(" ++ show from ++ " -> " ++ show to ++ ")"

data ForAll = ForAll [Name] UType deriving (Show)
data TypeOrForall = UT UType
                  | FA ForAll
                  deriving (Show)
type UEnv = Map.Map Name TypeOrForall
type Substitution = Map.Map Name UType

infer :: [AST.StatementSpan] -> UnificationMonad (UType, Substitution)
infer [] = return (TNamed _void, Map.empty)
infer (AST.Fun (AST.Ident ident _) args _ body _ : rest) = do
    backupEnv <- gets env
    let names = map nameOfArg args
    argTypes <- newTVars (length names)
    addToContexts names (map UT argTypes)
    resType <- newTVar
    let funcType = makeFunType $ argTypes ++ [resType]
    addToContext ident (UT funcType)
    (bodyType, s1) <- infer body
    s2 <- unify (applySubstToType s1 resType) bodyType
    let s3 = composeSubst s1 s2
    let inferredType = applySubstToType s3 funcType

    modify (\st -> st {env = backupEnv})
    applySubstToCtx s3
    env <- gets env
    let inferredPolyType = generalize env inferredType
    addToContext ident inferredPolyType
    (restType, s4) <- infer rest
    let s5 = composeSubst s3 s4
    return (restType, s5)

    where
        makeFunType :: [UType] -> UType
        makeFunType [] = TNamed _void
        makeFunType [tvar] = tvar
        makeFunType (tvar : rest) = TFun tvar (makeFunType rest)
infer (AST.Assign [(AST.Var (AST.Ident name _) _)] rhs annot : rest) = do
    (rhsType, s1) <- inferExpr rhs
    applySubstToCtx s1
    env <- gets env
    let rhsPolyType = generalize env rhsType
    addToContext (name) rhsPolyType
    (bodyType, s2) <- infer rest
    let s3 = composeSubst s1 s2
    return (bodyType, s3)
infer (AST.Conditional [(cond, trueBranch)] falseBranch annot : rest) = do
    (condType, s0) <- inferExpr cond
    s1 <- unify condType (TNamed bool)
    applySubstToCtx (composeSubst s0 s1)
    (_trueBranchType, s2) <- infer trueBranch
    let s3 = composeSubst (composeSubst s0 s1) s2
    applySubstToCtx s3
    (_falseBranchType, s4) <- infer falseBranch
    let s5 = composeSubst s3 s4

    let trueBranchType = applySubstToType s5 _trueBranchType
    let falseBranchType = applySubstToType s5 _falseBranchType
    s6 <- unify trueBranchType falseBranchType
    let resultSubst = composeSubst s5 s6
    return (applySubstToType s6 trueBranchType, resultSubst)
infer (AST.Return expr annot : rest) = 
    case expr of
        Nothing -> return (TNamed _void, Map.empty)
        Just exp -> inferExpr exp

inferExpr :: AST.ExprSpan -> UnificationMonad (UType, Substitution)
inferExpr (AST.Var (AST.Ident ident _) annot) = do
    env <- gets env
    case Map.lookup ident env of
        Nothing -> throwError $ "Unbound var: " ++ ident
        Just envType -> case envType of
            UT typ -> return (typ, Map.empty)
            FA fa -> do
                inst <- instantiate fa
                return (inst, Map.empty)
inferExpr (AST.Int value lit annot) = return (TNamed int, Map.empty)
inferExpr (AST.LongInt value lit annot) = return (TNamed int, Map.empty)
inferExpr (AST.Float value lit annot) = return (TNamed float, Map.empty)
inferExpr (AST.Imaginary value lit annot) = return (TNamed complex, Map.empty)
inferExpr (AST.Bool value annot) = return (TNamed bool, Map.empty)
inferExpr (AST.Strings strings annot) = return (TNamed str, Map.empty)
inferExpr (AST.Call func args annot) = do
    (funcType, s1) <- inferExpr func
    applyFunction funcType s1 (map argToExpr args)
    where
        argToExpr :: AST.ArgumentSpan -> AST.ExprSpan
        argToExpr (AST.ArgExpr expr _) = expr
inferExpr (AST.Paren expr _) = inferExpr expr
inferExpr (AST.BinaryOp operator larg rarg annot) = do
    (funcType, s1) <- inferExpr (getUFunction operator)
    applyFunction funcType Map.empty [larg, rarg]
inferExpr expr = throwError $ show expr

applyFunction :: UType -> Substitution -> [AST.ExprSpan] -> UnificationMonad (UType, Substitution)
applyFunction funcType subst [] = return (funcType, subst)
applyFunction funcType s1 (arg : re) = do
    applySubstToCtx s1
    (argType, s2) <- inferExpr arg
    newVar <- newTVar
    let s3 = composeSubst s1 s2
    s4 <- unify (TFun argType newVar) funcType
    let funcType1@(TFun from to) = applySubstToType s4 funcType
    let s5 = composeSubst s3 s4
    s6 <- unify (applySubstToType s5 from) argType
    let resultSubst = composeSubst s5 s6
    applyFunction (applySubstToType s6 to) resultSubst re

getUFunction :: AST.OpSpan -> AST.ExprSpan
getUFunction (AST.Plus _) = AST.Var (AST.Ident "-plus" blankAnnot) blankAnnot -- TFun (TNamed int) (TFun (TNamed int) (TNamed int))
getUFunction (AST.Minus _) = AST.Var (AST.Ident "-minus" blankAnnot) blankAnnot -- TFun (TNamed int) (TFun (TNamed int) (TNamed int))
getUFunction (AST.Multiply _) = AST.Var (AST.Ident "-multiply" blankAnnot) blankAnnot -- TFun (TNamed int) (TFun (TNamed int) (TNamed int))
getUFunction (AST.LessThanEquals _) = AST.Var (AST.Ident "-leq" blankAnnot) blankAnnot -- TFun (TVar "A") (TFun (TVar "A") (TNamed bool))

-- replace type variables in type if they are present in substitution
applySubstToType :: Substitution -> UType -> UType
applySubstToType subst t@(TNamed _) = t
applySubstToType subst var@(TVar name) = 
    case Map.lookup name subst of
        Just t -> t
        Nothing -> var
applySubstToType subst (TFun from to) = TFun (applySubstToType subst from) (applySubstToType subst to)

-- same but with forall
applySubstToForAll :: Substitution -> ForAll -> ForAll
applySubstToForAll subst (ForAll names t) = ForAll names (applySubstToType substWithoutBounds t) where
    substWithoutBounds :: Substitution
    substWithoutBounds = foldr (\name acc -> Map.delete name acc) subst names 

applySubstTo :: Substitution -> TypeOrForall -> TypeOrForall
applySubstTo subst (UT typ) = UT (applySubstToType subst typ)
applySubstTo subst (FA fa) = FA (applySubstToForAll subst fa)

addToContexts :: [Name] -> [TypeOrForall] -> UnificationMonad ()
addToContexts [] [] = return ()
addToContexts (name : rn) (tf : rtf) = do
    addToContext name tf
    addToContexts rn rtf

addToContext :: Name -> TypeOrForall -> UnificationMonad ()
addToContext name tof = do
    ctxe <- gets env
    modify (\st -> st { env = Map.insert name tof ctxe })
    return ()

newTVars :: Int -> UnificationMonad [UType]
newTVars 1 = do
    newVar <- newTVar
    return [newVar]
newTVars n = do
    tvar <- newTVar
    rest <- newTVars (n - 1)
    return $ tvar : rest

newTVar :: UnificationMonad UType
newTVar = do
    n <- gets next
    let tvar = 'T' : show n
    modify (\st -> st { next = next st + 1})
    return $ TVar tvar

unify :: UType -> UType -> UnificationMonad Substitution
unify (TNamed t1) (TNamed t2) = 
    if t1 == t2 then 
        return $ Map.empty 
    else 
        throwError $ "Mismatch between (" ++ show t1 ++ ") and (" ++ show t2 ++ ")"
unify (TVar t1) t2 = varBind t1 t2
unify t1 (TVar t2) = varBind t2 t1
unify (TFun from1 to1) (TFun from2 to2) = do
    s1 <- unify from1 from2
    s2 <- unify (applySubstToType s1 to1) (applySubstToType s1 to2)
    return $ composeSubst s1 s2
unify t1 t2 = throwError $ "Weird error in unification which shouldn't happen"

composeSubst :: Substitution -> Substitution -> Substitution
composeSubst s1 s2 = Map.union result s1 where
    result = foldr (\(name, t) acc -> Map.insert name (applySubstToType s1 t) acc) Map.empty (Map.assocs s2)

varBind :: Name -> UType -> UnificationMonad Substitution
varBind name (TVar name2) = 
    if name == name2 then 
        return $ Map.empty 
    else 
        varBind2 name (TVar name2)
varBind name t = varBind2 name t

varBind2 :: Name -> UType -> UnificationMonad Substitution
varBind2 name t = 
    if contains t name then
        throwError $ "Type " ++ show t ++ " contains a reference to itself"
    else
        return $ Map.insert name t (Map.empty)


contains :: UType -> Name -> Bool
contains typ name = 
    case typ of
        TNamed _ -> False
        TVar name2 -> name == name2
        TFun from to -> contains from name || contains to name


applySubstToCtx :: Substitution -> UnificationMonad ()
applySubstToCtx subst = do
    env <- gets env
    let newEnv = foldr (\(name, typ) acc -> Map.insert name (applySubstTo subst typ) acc) env (Map.assocs env)
    modify (\st -> st { env = newEnv})
    

type FreeVars = [Name]

unionFV :: FreeVars -> FreeVars -> FreeVars
unionFV a b = nub $ a ++ b

difference :: FreeVars -> FreeVars -> FreeVars
difference a b = filter (\e -> e `notElem` b) a

freeTypeVarsInType :: UType -> FreeVars
freeTypeVarsInType (TNamed _ ) = []
freeTypeVarsInType (TVar name) = [name]
freeTypeVarsInType (TFun t1 t2) = unionFV (freeTypeVarsInType t1) (freeTypeVarsInType t2)

freeTypeVarsInForall :: ForAll -> FreeVars
freeTypeVarsInForall (ForAll quantifiers t) = difference freeInType quantifiers where
    freeInType = freeTypeVarsInType t

freeTypeVarsIn :: TypeOrForall -> FreeVars
freeTypeVarsIn (UT t) = freeTypeVarsInType t
freeTypeVarsIn (FA fa) = freeTypeVarsInForall fa

freeTypeVarsInEnv :: UEnv -> FreeVars
freeTypeVarsInEnv env = foldr (\(name, typ) acc -> unionFV acc (freeTypeVarsIn typ)) [] (Map.assocs env)

instantiate :: ForAll -> UnificationMonad UType
instantiate (ForAll quantifiers t) = do
    subst <- newSubst quantifiers
    return $ applySubstToType subst t 
    where
        newSubst :: [Name] -> UnificationMonad Substitution
        newSubst [] = return Map.empty
        newSubst (quant : rq) = do
            tvar <- newTVar
            subst <- newSubst rq
            return $ Map.insert quant tvar subst

inferVar :: AST.ExprSpan -> UnificationMonad (UType, Substitution)
inferVar (AST.Var (AST.Ident ident _) _) = do
    env <- gets env
    case Map.lookup ident env of
        Just envType -> case envType of
            UT typ -> return (typ, Map.empty)
            FA fa -> do
                newType <- instantiate fa
                return (newType, Map.empty)
        Nothing -> throwError $ "Unbound var: " ++ ident

generalize :: UEnv -> UType -> TypeOrForall
generalize env t = if length quantifiers > 0 then FA (ForAll quantifiers t) else UT t where
    envFreeVars = freeTypeVarsInEnv env
    typeFreeVars = freeTypeVarsInType t
    quantifiers = difference typeFreeVars envFreeVars