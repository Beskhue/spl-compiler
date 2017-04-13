{-# OPTIONS_GHC -XFlexibleInstances #-}

{-|
Module: TypeSystem.Checker
Description: A type checker for SPL
Copyright: (c) Thomas Churchman, 2017
License: MIT
Maintainer: thomas@kepow.org
Stability: experimental

A type checker based on Algorithm W.

See more here: http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.65.7733&rep=rep1&type=pdf (last retrieved
10 April 2017).
https://github.com/mgrabmueller/AlgorithmW

-}

module TypeSystem.Checker where

import qualified Debug.Trace as Trace

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Stack as Stack

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State

import qualified Data.Pos as Pos
import qualified Data.AST as AST

------------------------------------------------------------------------------------------------------------------------

check :: AST.SPL -> Either String AST.SPL
check spl =
    case res of
        Left err -> Left err
        Right (spl, _, _) -> Right spl
    where
        (res, _) = runTInf $ tInfSPL emptyScopedCtx spl

checkDet :: AST.SPL -> AST.SPL
checkDet spl =
    case check spl of
        Left err -> Trace.trace (show err) undefined
        Right spl -> spl

typeInferenceDet :: (ScopedTypeCtx -> a -> TInf (Substitution, Type)) -> ScopedTypeCtx -> a -> (Substitution, Type)
typeInferenceDet tInf' ctx e =
    case typeInference tInf' ctx e of
        Left err -> Trace.trace (show err) undefined
        Right t -> t

onlyType :: Either String (Substitution, Type) -> Either String Type
onlyType res =
    case res of
        Left err -> Left err
        Right (_, t) -> Right t

typeInference :: (ScopedTypeCtx -> a -> TInf (Substitution, Type)) -> ScopedTypeCtx -> a -> Either String (Substitution, Type)
typeInference tInf' ctx e = res
    where
        (res, _) = runTInf $ tInf' ctx e

typeInferenceExpr :: ScopedTypeCtx -> AST.Expression -> Either String (Substitution, Type)
typeInferenceExpr = typeInference tInfExpr

------------------------------------------------------------------------------------------------------------------------

-- |The possible types
data Type = TVar String
          | TBool
          | TInt
          | TChar
          | TList Type
          | TTuple Type Type
          | TFunction [Type] Type
          | TVoid
            deriving (Show, Eq, Ord)

-- |A type scheme (polytype): a type with a list of bound type variables (the type variables not bound are still free)
data Scheme = Scheme [String] Type
              deriving (Show)

------------------------------------------------------------------------------------------------------------------------

-- |Class to find free type variables and to apply type substitutions
class Types a where
    freeTypeVars :: a -> Set.Set String
    apply :: Substitution -> a -> a
    applyOnlyRename :: Substitution -> a -> a

instance Types Type where
    freeTypeVars (TVar v) = Set.singleton v
    freeTypeVars TBool = Set.empty
    freeTypeVars TInt = Set.empty
    freeTypeVars TChar = Set.empty
    freeTypeVars (TList l) = freeTypeVars l
    freeTypeVars (TTuple t1 t2) = Set.union (freeTypeVars t1) (freeTypeVars t2)
    freeTypeVars (TFunction args body) = Set.union (freeTypeVars args) (freeTypeVars body)
    freeTypeVars TVoid = Set.empty

    apply s (TVar v) =
        case Map.lookup v s of
            Just t -> t
            _ -> TVar v
    apply s (TList l) = TList $ apply s l
    apply s (TTuple t1 t2) = TTuple (apply s t1) (apply s t2)
    apply s (TFunction arg body) = TFunction (apply s arg) (apply s body)
    apply s t = t

    applyOnlyRename s (TVar v) =
        case Map.lookup v s of
            Just (TVar v') -> TVar v'
            _ -> TVar v
    applyOnlyRename s (TList l) = TList $ applyOnlyRename s l
    applyOnlyRename s (TTuple t1 t2) = TTuple (applyOnlyRename s t1) (applyOnlyRename s t2)
    applyOnlyRename s (TFunction arg body) = TFunction (applyOnlyRename s arg) (applyOnlyRename s body)
    applyOnlyRename s t = t

instance Types Scheme where
    freeTypeVars (Scheme vars t) = (freeTypeVars t) `Set.difference` (Set.fromList vars)
    apply s (Scheme vars t) = Scheme vars (apply (foldr Map.delete s vars) t)
    applyOnlyRename s (Scheme vars t) = Scheme vars (applyOnlyRename (foldr Map.delete s vars) t)

-- |Might be useful to have the class available on lists as well
instance Types a => Types [a] where
    freeTypeVars l = foldr Set.union Set.empty (map freeTypeVars l)
    apply s = map (apply s)
    applyOnlyRename s = map (applyOnlyRename s)

------------------------------------------------------------------------------------------------------------------------

-- |A substitution is a (finite) mapping from type variables to types
type Substitution = Map.Map String Type

nullSubstitution :: Substitution
nullSubstitution = Map.empty

-- |Compose two substitutions: substitute the types in s2 using the s1 substitution, and merge the resulting
-- substitution back with s1
composeSubstitution :: Substitution -> Substitution -> Substitution
composeSubstitution s1 s2 = (Map.map (apply s1) s2) `Map.union` s1

------------------------------------------------------------------------------------------------------------------------

-- |A type context (or environment) is a mapping from term variables to type schemes
newtype TypeCtx = TypeCtx (Map.Map String Scheme)
                  deriving (Show)

type ScopedTypeCtx = Stack.Stack TypeCtx

emptyMap :: Map.Map String Scheme
emptyMap = Map.empty

emptyCtx :: TypeCtx
emptyCtx = TypeCtx emptyMap

emptyScopedCtx :: ScopedTypeCtx
emptyScopedCtx = Stack.stackNew

class TypingEnvironment a where
    add :: a -> String -> Scheme ->  a
    remove :: a -> String -> a

instance TypingEnvironment TypeCtx where
    --add (TypeCtx ctx) var scheme = TypeCtx (ctx `Map.union` (Map.singleton var scheme))
    add (TypeCtx ctx) var scheme = TypeCtx (Map.insert var scheme ctx)
    remove (TypeCtx ctx) var = TypeCtx (Map.delete var ctx)

instance TypingEnvironment a => TypingEnvironment (Stack.Stack a) where
    add stack var scheme =
        case Stack.stackPop stack of
            Just (stack', a) -> Stack.stackPush stack' (add a var scheme)
            _ -> stack
    remove stack var =
        case Stack.stackPop stack of
            Just (stack', a) -> Stack.stackPush stack' (remove a var)
            _ -> stack

instance Types TypeCtx where
    freeTypeVars (TypeCtx ctx) = freeTypeVars (Map.elems ctx)
    apply s (TypeCtx ctx) = TypeCtx (Map.map (apply s) ctx)
    applyOnlyRename s (TypeCtx ctx) = TypeCtx (Map.map (applyOnlyRename s) ctx)

instance Types a => Types (Stack.Stack a) where
    freeTypeVars stack =
        case Stack.stackPop stack of
            Just (stack', a) -> freeTypeVars a `Set.union` freeTypeVars stack'
            _ -> Set.empty
    apply s stack =
        case Stack.stackPop stack of
            --Just (stack', a) -> Stack.stackPush stack' (apply s a)
            Just (stack', a) -> Stack.stackPush (apply s stack') (apply s a)
            _ -> stack
    applyOnlyRename s stack =
        case Stack.stackPop stack of
            --Just (stack', a) -> Stack.stackPush stack' (applyOnlyRename s a)
            Just (stack', a) -> Stack.stackPush (applyOnlyRename s stack') (applyOnlyRename s a)
            _ -> stack

------------------------------------------------------------------------------------------------------------------------

-- |Abstract a type over all type variables free in the type but not free in the context, i.e. creates a type scheme
-- for a type in a given context, by binding all type variables in the type that are not bound in the context to the
-- type.
generalize :: ScopedTypeCtx -> Type -> Scheme
generalize ctx t =
    let vars = Set.toList ((freeTypeVars t) `Set.difference` (freeTypeVars ctx)) in
        Scheme vars t

------------------------------------------------------------------------------------------------------------------------

-- |The type inference context (shared in TInf using ReaderT)
data TInfCtx = TInfCtx {}

-- |The type inference state consists of the fresh type name generator state and the current type substitution
data TInfState = TInfState { tInfSupply :: Int,
                             tInfSubst :: Substitution}
                 deriving (Show)

-- type TInf a = ExceptT String (ReaderT TInfCtx (StateT TInfState IO)) a
type TInf a = ExceptT String (ReaderT TInfCtx (State TInfState)) a

-- |The type inference runner. Results in a tuple of either an error and the type inference result, and the final type
-- inference state. The type inference result will generally be a tuple of a substitution (the type constraints) and the
-- type of an AST branch.
runTInf :: TInf a -> (Either String a, TInfState)
runTInf t = runState (runReaderT (runExceptT t) initTInfCtx) initTInfState
    where
        initTInfCtx = TInfCtx{}
        initTInfState = TInfState { tInfSupply = 0,
                                    tInfSubst = Map.empty}

------------------------------------------------------------------------------------------------------------------------

-- |Generate a new type variable using the type name supplier
newTypeVar :: String -> TInf Type
newTypeVar suffix = do
    s <- get
    put s {tInfSupply = tInfSupply s + 1}
    return (TVar ("t_" ++ show (tInfSupply s) ++ suffix))

-- |Replace bound type variables in a scheme with fresh type variables
instantiate :: Scheme -> TInf Type
instantiate (Scheme vars t) = do
    newVars <- mapM (\_ -> newTypeVar "a") vars
    let s = Map.fromList (zip vars newVars)
    return $ apply s t

------------------------------------------------------------------------------------------------------------------------

-- |Bind a type variable to a type, but don't bind to itself, and make sure the free type variable occurs
varBind :: String -> Type -> TInf Substitution
varBind u t
    | t == TVar u                   = return nullSubstitution
    | u `Set.member` freeTypeVars t = throwError $ "occurs check fails: " ++ u ++ " vs. " ++ show t
    | otherwise                     = return (Map.singleton u t)

-- |Unify two types (using the most general unifier)
mgu :: Type -> Type -> TInf Substitution
mgu (TVar u) t           = varBind u t
mgu t (TVar u)           = varBind u t
mgu TBool TBool          = return nullSubstitution
mgu TInt TInt            = return nullSubstitution
mgu TChar TChar          = return nullSubstitution
mgu (TList t) (TList t') = mgu t t'
mgu (TTuple t1 t2) (TTuple t1' t2') = do
    s1 <- mgu t1 t1'
    s2 <- mgu (apply s1 t2) (apply s1 t2')
    return $ s1 `composeSubstitution` s2
mgu (TFunction [] body) (TFunction [] body') = mgu body body'
mgu (TFunction (arg:args) body) (TFunction (arg':args') body') = do
    s1 <- mgu arg arg'
    s2 <- mgu (apply s1 (TFunction args body)) (apply s1 (TFunction args' body'))
    return $ s2 `composeSubstitution` s1
mgu TVoid TVoid          = return nullSubstitution
mgu t1 t2                = throwError $ "types do not unify: " ++ show t1 ++ " and " ++ show t2

------------------------------------------------------------------------------------------------------------------------

-- |Get the name of an AST identifier
idName :: AST.Identifier -> String
idName (AST.Identifier i, _) = i

getScheme :: ScopedTypeCtx -> String -> TInf Scheme
getScheme stack varName = getScheme' (Stack.stackToList stack) varName
    where
        getScheme' :: [TypeCtx] -> String -> TInf Scheme
        getScheme' [] _ = throwError $ "unbound variable: " ++ varName
        getScheme' (TypeCtx ctx : ctxs) varName =
            case Map.lookup varName ctx of
                Nothing -> getScheme' ctxs varName
                Just scheme -> return scheme

tInfVarName :: ScopedTypeCtx -> String -> TInf Type
tInfVarName stack varName = tInfVarName' (Stack.stackToList stack) varName
    where
        tInfVarName' :: [TypeCtx] -> String -> TInf Type
        tInfVarName' [] _ = throwError $ "unbound variable: " ++ varName
        tInfVarName' (TypeCtx ctx : ctxs) varName =
            case Map.lookup varName ctx of
                Nothing -> tInfVarName' ctxs varName
                Just scheme -> instantiate scheme

-- |Perform type inference on an AST identifier
tInfId :: ScopedTypeCtx -> AST.Identifier -> TInf (Substitution, Type)
tInfId stack i = do
    t <- tInfVarName stack (idName i)
    return (nullSubstitution, t)

-- |Perform type inference on an AST constant
tInfConst :: ScopedTypeCtx -> AST.Constant -> TInf (Substitution, Type)
tInfConst _ (AST.ConstBool _, _) = return (nullSubstitution, TBool)
tInfConst _ (AST.ConstInt _, _) = return (nullSubstitution, TInt)
tInfConst _ (AST.ConstChar _, _) = return (nullSubstitution, TChar)
tInfConst _ (AST.ConstEmptyList, _) = do
    tVar <- newTypeVar "a"
    return (nullSubstitution, TList tVar)

-- |Perform type inference on an AST expression
tInfExpr :: ScopedTypeCtx -> AST.Expression -> TInf (Substitution, Type)
tInfExpr ctx (AST.ExprIdentifier id, _) = tInfId ctx id
tInfExpr ctx (AST.ExprIdentifierField id fields, _) = do
    (s, t) <- tInfId ctx id
    (s', t') <- tTraverseFields ctx t fields
    return (s' `composeSubstitution` s, t')
    where
        tTraverseFields :: ScopedTypeCtx -> Type -> [AST.Field] -> TInf (Substitution, Type)
        tTraverseFields _ t [] = return (nullSubstitution, t)
        tTraverseFields ctx t (field:fields) =
            case field of
                (AST.FieldHd, _) -> do
                    tVar <- newTypeVar "fld"
                    s <- mgu t (TList tVar)
                    (s', t') <- tTraverseFields (apply s ctx) (apply s tVar) fields
                    return (s' `composeSubstitution` s, t')
                (AST.FieldTl, _) -> do
                    tVar <- newTypeVar "fld"
                    s <- mgu t (TList tVar)
                    (s', t') <- tTraverseFields (apply s ctx) (apply s (TList tVar)) fields
                    return (s' `composeSubstitution` s, t')
                (AST.FieldFst, _) -> undefined
                (AST.FieldSnd, _) -> undefined

tInfExpr ctx (AST.ExprFunCall id args, _) = do
    tReturn <- newTypeVar (idName id ++ "_ret")
    (s1, t1) <- tInfId ctx id
    (s2, ts) <- tInfExprs (apply s1 ctx) args
    s <- mgu t1 (TFunction ts tReturn)
    return (s2 `composeSubstitution` s1, apply s tReturn)
tInfExpr ctx (AST.ExprConstant const, _) = tInfConst ctx const
tInfExpr ctx (AST.ExprTuple e1 e2, _) = tInfTuple ctx e1 e2
tInfExpr ctx (AST.ExprUnaryOp op e, _) = tInfUnaryOp ctx op e
tInfExpr ctx (AST.ExprBinaryOp op e1 e2, _) = tInfBinaryOp ctx op e1 e2

-- |Perform type inference on a list of AST expressions
tInfExprs :: ScopedTypeCtx -> [AST.Expression] -> TInf (Substitution, [Type])
tInfExprs _ [] = return (nullSubstitution, [])
tInfExprs ctx (expr:exprs) = do
    (s1, t) <- tInfExpr ctx expr
    (s2, ts) <- tInfExprs (apply s1 ctx) exprs

    return (s2 `composeSubstitution` s1, apply s2 t : ts)

tInfTuple :: ScopedTypeCtx -> AST.Expression -> AST.Expression -> TInf (Substitution, Type)
tInfTuple ctx e1 e2 = do
    (s1, t1) <- tInfExpr ctx e1
    (s2, t2) <- tInfExpr (apply s1 ctx) e2
    return (s2 `composeSubstitution` s1, TTuple (apply s2 t1) t2)

tInfUnaryOp :: ScopedTypeCtx -> AST.UnaryOperator -> AST.Expression -> TInf (Substitution, Type)
tInfUnaryOp ctx (AST.UnaryOpNeg, _) e = do
    (s1, t1) <- tInfExpr ctx e
    s <- mgu t1 TBool
    return (s `composeSubstitution` s1, TBool)
tInfUnaryOp ctx (AST.UnaryOpSubtr, _) e = do
    (s1, t1) <- tInfExpr ctx e
    s <- mgu t1 TInt
    return (s `composeSubstitution` s1, TInt)

tInfBinaryOp :: ScopedTypeCtx -> AST.BinaryOperator -> AST.Expression -> AST.Expression -> TInf (Substitution, Type)
tInfBinaryOp ctx (AST.BinaryOpOr, _) e1 e2 = do
    (s1, t1) <- tInfExpr ctx e1
    s1' <- mgu t1 TBool

    (s2, t2) <- tInfExpr (apply (s1' `composeSubstitution` s1) ctx) e2
    s2' <- mgu (apply (s2 `composeSubstitution` s1') t1) t2

    return (s2' `composeSubstitution` s2 `composeSubstitution` s1' `composeSubstitution` s1, TBool)
tInfBinaryOp ctx (AST.BinaryOpAnd, p) e1 e2 = tInfBinaryOp ctx (AST.BinaryOpOr, p) e1 e2
tInfBinaryOp ctx (AST.BinaryOpEq, _) e1 e2 = do
    (s1, t1) <- tInfExpr ctx e1
    (s2, t2) <- tInfExpr (apply s1 ctx) e2
    s <- mgu (apply s2 t1) t2
    return (s `composeSubstitution` s2 `composeSubstitution` s1, TBool)
tInfBinaryOp ctx (AST.BinaryOpNEq, p) e1 e2 = tInfBinaryOp ctx (AST.BinaryOpEq, p) e1 e2
tInfBinaryOp ctx (AST.BinaryOpLT, _) e1 e2 = do
    (s1, t1) <- tInfExpr ctx e1
    s1' <- mgu t1 TInt

    (s2, t2) <- tInfExpr (apply (s1' `composeSubstitution` s1) ctx) e2
    s2' <- mgu (apply (s2 `composeSubstitution` s1') t1) t2

    return (s2' `composeSubstitution` s2 `composeSubstitution` s1' `composeSubstitution` s1, TBool)
tInfBinaryOp ctx (AST.BinaryOpGT, p) e1 e2 = tInfBinaryOp ctx (AST.BinaryOpLT, p) e1 e2
tInfBinaryOp ctx (AST.BinaryOpLTE, p) e1 e2= tInfBinaryOp ctx (AST.BinaryOpLT, p) e1 e2
tInfBinaryOp ctx (AST.BinaryOpGTE, p) e1 e2 = tInfBinaryOp ctx (AST.BinaryOpLT, p) e1 e2
tInfBinaryOp ctx (AST.BinaryOpConcat, _) e1 e2 = do
    (s1, t1) <- tInfExpr ctx e1
    (s2, t2) <- tInfExpr (apply s1 ctx) e2
    s <- mgu (TList (apply s2 t1)) t2
    return (s `composeSubstitution` s2 `composeSubstitution` s1, apply s t2)
tInfBinaryOp ctx (AST.BinaryOpPlus, _) e1 e2 = do
    (s1, t1) <- tInfExpr ctx e1
    s1' <- mgu t1 TInt

    (s2, t2) <- tInfExpr (apply (s1' `composeSubstitution` s1) ctx) e2
    s2' <- mgu (apply (s2 `composeSubstitution` s1') t1) t2

    return (s2' `composeSubstitution` s2 `composeSubstitution` s1' `composeSubstitution` s1, TInt)
tInfBinaryOp ctx (AST.BinaryOpSubtr, p) e1 e2 = tInfBinaryOp ctx (AST.BinaryOpPlus, p) e1 e2
tInfBinaryOp ctx (AST.BinaryOpMult, p) e1 e2 = tInfBinaryOp ctx (AST.BinaryOpPlus, p) e1 e2
tInfBinaryOp ctx (AST.BinaryOpDiv, p) e1 e2 = tInfBinaryOp ctx (AST.BinaryOpPlus, p) e1 e2
tInfBinaryOp ctx (AST.BinaryOpMod, p) e1 e2 = tInfBinaryOp ctx (AST.BinaryOpPlus, p) e1 e2

------------------------------------------------------------------------------------------------------------------------

-- |Perform type inference on the global SPL declarations. Rewrite the AST such that all variables are typed as
-- completely as possible.
tInfSPL :: ScopedTypeCtx -> AST.SPL -> TInf (AST.SPL, Substitution, Type)
tInfSPL ctx decls = do
    let initCtx = Stack.stackPush ctx emptyCtx -- Create top scope
    ctx' <- addGlobalsToCtx initCtx decls -- Add globals to to scope
    tInfSPL' ctx' decls
    where
        addGlobalsToCtx :: ScopedTypeCtx -> AST.SPL -> TInf ScopedTypeCtx
        addGlobalsToCtx ctx [] = return ctx
        addGlobalsToCtx ctx (decl:decls) = do
            typeVar <- newTypeVar "global" -- Create a (temporary) new type var for this global
            ctx' <- addGlobalsToCtx ctx decls
            case decl of
                (AST.DeclV (AST.VarDeclTyped _ i _, _), _) -> return $ add ctx' (idName i) (Scheme [] typeVar)
                (AST.DeclV (AST.VarDeclUntyped i _, _), _) -> return $ add ctx' (idName i) (Scheme [] typeVar)
                --(AST.DeclF (AST.FunDeclTyped i _ _ _, _), _) -> return $ add ctx' (idName i) (generalize ctx' typeVar)
                --(AST.DeclF (AST.FunDeclUntyped i _ _, _), _) -> return $ add ctx' (idName i) (generalize ctx' typeVar)
                (AST.DeclF (AST.FunDeclTyped i _ _ _, _), _) -> return $ add ctx' (idName i) (Scheme [] typeVar)
                (AST.DeclF (AST.FunDeclUntyped i _ _, _), _) -> return $ add ctx' (idName i) (Scheme [] typeVar)

        tInfSPL' :: ScopedTypeCtx -> AST.SPL -> TInf (AST.SPL, Substitution, Type)
        tInfSPL' _ [] = return ([], nullSubstitution, TVoid)
        tInfSPL' ctx (decl:decls) = do
            -- Infer type of the declaration
            (decl', s1, varName, t1) <- tInfDecl ctx decl

            -- Get the type scheme of the variable/function we just declared and unify it with the actual type
            (Scheme _ t1') <- getScheme ctx varName
            s2 <- mgu t1' t1

            -- Get the next declarations, using the unified type scheme
            (decls', s3, t2) <- tInfSPL' (apply (s2 `composeSubstitution` s1) ctx) decls

            -- Infer type for this declaration again, using the "back-flown" information from the next declarations
            --(declPost, s1Post, varNamePost, t1Post) <- tInfDecl (apply s3 ctx) decl

            return (
                        apply (s3 `composeSubstitution` s2 ) decl' : decls',
                        s3 `composeSubstitution` s2 `composeSubstitution` s1,
                        t2
                    )

tInfDecl :: ScopedTypeCtx -> AST.Decl -> TInf (AST.Decl, Substitution, String, Type)
tInfDecl ctx (AST.DeclV decl, p) = do
    (decl', s, varName, t) <- tInfVarDecl ctx decl
    return ((AST.DeclV decl', p), s, varName, t)
tInfDecl ctx (AST.DeclF decl, p) = do
    (decl', s, varName, t) <- tInfFunDecl ctx decl
    return ((AST.DeclF decl', p), s, varName, t)

tInfVarDecl :: ScopedTypeCtx -> AST.VarDecl -> TInf (AST.VarDecl, Substitution, String, Type)
tInfVarDecl ctx decl =
    case decl of
        (AST.VarDeclTyped annotatedType identifier expr, p) ->
            let annotatedT = rTranslateType annotatedType in do
            (ast, s, str, t) <- tInfVarDecl' p ctx identifier expr
            s' <- mgu annotatedT t `catchError` (\_ ->
                throwError $ "Could not unify types"
                    ++ ". Expected type: " ++ AST.prettyPrint (translateType p annotatedT)
                    ++ ". Inferred type: " ++ AST.prettyPrint (translateType p t) ++ ".")
            if apply s' annotatedT == applyOnlyRename s' annotatedT
                then return (apply s' ast, s' `composeSubstitution` s, str, apply s' t)
                else throwError $ "Expected type is more general than the inferred type"
                    ++ ". Expected type: " ++ AST.prettyPrint (translateType p annotatedT)
                    ++ ". Inferred type: " ++ AST.prettyPrint (translateType p t) ++ "."
        (AST.VarDeclUntyped identifier expr, p) -> tInfVarDecl' p ctx identifier expr
    where
        tInfVarDecl' :: Pos.Pos -> ScopedTypeCtx -> AST.Identifier -> AST.Expression -> TInf (AST.VarDecl, Substitution, String, Type)
        tInfVarDecl' p ctx identifier expr = do
            (s, t) <- tInfExpr ctx expr
            let t' = translateType p t
            return ((AST.VarDeclTyped t' identifier expr, p), s, idName identifier, t)

tInfFunDecl :: ScopedTypeCtx -> AST.FunDecl -> TInf (AST.FunDecl, Substitution, String, Type)
tInfFunDecl ctx decl =
    case decl of
        (AST.FunDeclTyped identifier args annotatedType stmts, p) ->
            let annotatedT = rTranslateFunType annotatedType in do
            (ast, s, str, t) <- tInfFunDecl' p ctx identifier args stmts
            s' <- mgu annotatedT t `catchError` (\_ ->
                throwError $ "Could not unify types"
                    ++ ". Expected type: " ++ AST.prettyPrint (translateFunType p annotatedT)
                    ++ ". Inferred type: " ++ AST.prettyPrint (translateFunType p t) ++ ".")
            if apply s' annotatedT == applyOnlyRename s' annotatedT
                then return (apply s' ast, s' `composeSubstitution` s, str, apply s' t)
                else throwError $ "Expected type is more general than the inferred type"
                    ++ ". Expected type: " ++ AST.prettyPrint (translateFunType p annotatedT)
                    ++ ". Inferred type: " ++ AST.prettyPrint (translateFunType p t) ++ "."
        (AST.FunDeclUntyped identifier args stmts, p) -> tInfFunDecl' p ctx identifier args stmts
    where
        tInfFunDecl' :: Pos.Pos -> ScopedTypeCtx -> AST.Identifier -> [AST.Identifier] -> [AST.Statement] -> TInf (AST.FunDecl, Substitution, String, Type)
        tInfFunDecl' p ctx identifier args stmts = do
            let newCtx = Stack.stackPush ctx emptyCtx
            scopedCtx <- addArgsToCtx (idName identifier ++ "_") newCtx args -- Create the function's scoped context
            (stmts', s, t) <- tInfStatements scopedCtx stmts

            argsTypes <- getArgsTypes (apply s scopedCtx) args
            let funType = TFunction argsTypes t
            let funTypeAST = translateFunType p funType

            return ((AST.FunDeclTyped identifier args funTypeAST stmts', p), s, idName identifier, funType)
            where
                addArgsToCtx :: String -> ScopedTypeCtx -> [AST.Identifier] -> TInf ScopedTypeCtx
                addArgsToCtx prefix ctx [] = return ctx
                addArgsToCtx prefix ctx (arg:args) = do
                    typeVar <- newTypeVar (prefix ++ "arg")
                    ctx' <- addArgsToCtx prefix ctx args
                    return $ add ctx' (idName arg) (Scheme [] typeVar)
                    -- return $ add ctx' (idName arg) (generalize ctx' typeVar)

                getArgsTypes :: ScopedTypeCtx -> [AST.Identifier] -> TInf [Type]
                getArgsTypes _ [] = return []
                getArgsTypes ctx (arg:args) = do
                    t <- tInfVarName ctx $ idName arg
                    ts <- getArgsTypes ctx args
                    return $ t:ts


tInfStatements :: ScopedTypeCtx -> [AST.Statement] -> TInf ([AST.Statement], Substitution, Type)
tInfStatements _ [] = return ([], nullSubstitution, TVoid)
tInfStatements ctx (statement:statements) = do
    (statement', s1, varName, t1, returnsValue) <- tInfStatement ctx statement

    -- Update local context if the statement declared a new variable
    let ctx' = (if varName == ""
                    then apply s1 ctx
                    else add (apply s1 ctx) varName (Scheme [] t1)
                )

    (statements', s2, t2) <- tInfStatements ctx' statements

    if returnsValue
        then
            case t2 of
                TVoid -> return (statement' : statements', s2 `composeSubstitution` s1, apply s2 t1)
                _ -> do
                    s <- mgu (apply s2 t1) t2
                    return (
                        apply (s `composeSubstitution` s2) statement' : statements',
                        s `composeSubstitution` s2 `composeSubstitution` s1,
                        apply s t2)
        else return (statement' : statements', s2 `composeSubstitution` s1, t2)

tInfStatement :: ScopedTypeCtx -> AST.Statement -> TInf (AST.Statement, Substitution, String, Type, Bool)
tInfStatement ctx (AST.StmtVarDecl decl, p) = do
    (decl', s, varName, t) <- tInfVarDecl ctx decl
    case Stack.stackPeek ctx of
        Just (TypeCtx ctx') ->
            if Map.member varName ctx'
                then throwError $ "Variable multiply defined in scope: " ++ varName ++ show p
                else return ((AST.StmtVarDecl decl', p), s, varName, t, False)

tInfStatement ctx (AST.StmtIf expr st, p) = do
    (s1, t1) <- tInfExpr ctx expr
    s <- mgu t1 TBool
    (st', s2, varName, t2, returnsValue) <- tInfStatement (apply (s `composeSubstitution` s1) ctx) st
    return ((AST.StmtIf expr st', p), s2 `composeSubstitution` s `composeSubstitution` s1, varName, t2, returnsValue)
tInfStatement ctx (AST.StmtIfElse expr st1 st2, p) = do
    (es, et1) <- tInfExpr ctx expr
    s <- mgu et1 TBool
    (st1', s1', varName, st1, returnsValue1) <- tInfStatement (apply (s `composeSubstitution` es) ctx) st1
    (st2', s2', varName, st2, returnsValue2) <- tInfStatement (apply (s1' `composeSubstitution` s `composeSubstitution` es) ctx) st2

    if returnsValue1
        then if returnsValue2
            then do
                s' <- mgu st1 st2
                return (
                    (AST.StmtIfElse expr (apply (s' `composeSubstitution` s2') st1') (apply s' st2'), p),
                    s' `composeSubstitution` s2' `composeSubstitution` s1' `composeSubstitution` s `composeSubstitution` es,
                    "",
                    apply s' st2,
                    True)
            else return (
                (AST.StmtIfElse expr (apply s2' st1') st2', p),
                s2' `composeSubstitution` s1' `composeSubstitution` s `composeSubstitution` es,
                "",
                apply s2' st1,
                True)
        else return (
            (AST.StmtIfElse expr (apply s2' st1') st2', p),
            s2' `composeSubstitution` s1' `composeSubstitution` s `composeSubstitution` es,
            "",
            apply s2' st1,
            False)
tInfStatement ctx (AST.StmtWhile expr st, p) = do
    (s1, t1) <- tInfExpr ctx expr
    s <- mgu t1 TBool
    (st', s2, varName, t2, returnsValue) <- tInfStatement (apply (s `composeSubstitution` s1) ctx) st
    return ((AST.StmtWhile expr st', p), s2 `composeSubstitution` s `composeSubstitution` s1, varName, t2, returnsValue)
tInfStatement ctx (AST.StmtBlock stmts, p) = do
    let newCtx = Stack.stackPush ctx emptyCtx
    (stmts, s, t) <- tInfStatements newCtx stmts
    case t of
        TVoid -> return ((AST.StmtBlock stmts, p), s, "", t, False)
        _ -> return ((AST.StmtBlock stmts, p), s, "", t, True)
tInfStatement ctx (AST.StmtAssignment identifier expr, p) = do
    (Scheme _ t) <- getScheme ctx (idName identifier)
    (s1, t1) <- tInfExpr ctx expr

    s <- mgu t t1

    let t' = translateType p (apply s t1)
    return ((AST.StmtAssignment identifier expr, p), s `composeSubstitution` s1, "", apply s t1, False)
tInfStatement ctx (AST.StmtAssignmentField identifier fields expr, p) = throwError "Assigning to fields is not supported"
tInfStatement ctx (AST.StmtFunCall identifier expressions, p) = do
    (s, t) <- tInfExpr ctx (AST.ExprFunCall identifier expressions, p)
    return ((AST.StmtFunCall identifier expressions, p), s, "", TVoid, False)
tInfStatement ctx (AST.StmtReturn expr, p) = do
    (s, t) <- tInfExpr ctx expr
    return ((AST.StmtReturn expr, p), s, "", t, True)
tInfStatement ctx (AST.StmtReturnVoid, p) = return ((AST.StmtReturnVoid, p), nullSubstitution, "", TVoid, True)

------------------------------------------------------------------------------------------------------------------------

translateType :: Pos.Pos -> Type -> AST.Type
translateType p (TVar str)         = (AST.TypeIdentifier (AST.Identifier str, p), p)
translateType p TBool              = (AST.TypeBool, p)
translateType p TInt               = (AST.TypeInt, p)
translateType p TChar              = (AST.TypeChar, p)
translateType p (TList t)          = (AST.TypeList $ translateType p t, p)
translateType p (TTuple t1 t2)     = (AST.TypeTuple (translateType p t1) (translateType p t2), p)

translateFunType :: Pos.Pos -> Type -> AST.FunType
translateFunType p (TFunction args TVoid) = (AST.FunTypeVoid (map (translateType p) args), p)
translateFunType p (TFunction args body) = (AST.FunType (map (translateType p) args) (translateType p body), p)

rTranslateType :: AST.Type -> Type
rTranslateType (AST.TypeIdentifier id, _) = TVar $ idName id
rTranslateType (AST.TypeBool, _)          = TBool
rTranslateType (AST.TypeInt, _)           = TInt
rTranslateType (AST.TypeChar, _)          = TChar
rTranslateType (AST.TypeList t, _)        = TList $ rTranslateType t
rTranslateType (AST.TypeTuple t1 t2, _)   = TTuple (rTranslateType t1) (rTranslateType t2)

rTranslateFunType :: AST.FunType -> Type
rTranslateFunType (AST.FunTypeVoid args, _)  = TFunction (map rTranslateType args) TVoid
rTranslateFunType (AST.FunType args body, _) = TFunction (map rTranslateType args) (rTranslateType body)

instance Types AST.Type where
    freeTypeVars t = freeTypeVars $ rTranslateType t
    apply s (t, p) = translateType p $ apply s $ rTranslateType (t, p)

    applyOnlyRename = undefined

instance Types AST.Decl where
    freeTypeVars = undefined
    apply s (AST.DeclV v, p) = (AST.DeclV (apply s v), p)
    apply s (AST.DeclF f, p) = (AST.DeclF (apply s f), p)
    applyOnlyRename = undefined

instance Types AST.VarDecl where
    freeTypeVars = undefined
    apply s (AST.VarDeclTyped t i e, p) = (AST.VarDeclTyped (apply s t) (apply s i) (apply s e), p)
    apply s (AST.VarDeclUntyped i e, p) = (AST.VarDeclUntyped (apply s i) (apply s e), p)
    applyOnlyRename = undefined

instance Types AST.FunDecl where
    freeTypeVars = undefined
    apply s (AST.FunDeclTyped i is t ss, p) = (AST.FunDeclTyped (apply s i) (apply s is) (apply s t) (apply s ss), p)
    apply s (AST.FunDeclUntyped i is ss, p) = (AST.FunDeclUntyped (apply s i) (apply s is) (apply s ss), p)
    applyOnlyRename = undefined

instance Types AST.FunType where
    freeTypeVars = undefined
    apply s (AST.FunType ts t, p) = (AST.FunType (apply s ts) (apply s t), p)
    apply s (AST.FunTypeVoid ts, p) = (AST.FunTypeVoid (apply s ts), p)
    applyOnlyRename = undefined

instance Types AST.Statement where
    freeTypeVars = undefined
    apply s (AST.StmtVarDecl v, p) = (AST.StmtVarDecl (apply s v), p)
    apply s (AST.StmtIf e st, p) = (AST.StmtIf (apply s e) (apply s st), p)
    apply s (AST.StmtIfElse e st1 st2, p) = (AST.StmtIfElse (apply s e) (apply s st1) (apply s st2), p)
    apply s (AST.StmtWhile e st, p) = (AST.StmtWhile (apply s e) (apply s st), p)
    apply s (AST.StmtBlock sts, p) = (AST.StmtBlock (apply s sts), p)
    apply s (AST.StmtAssignment i e, p) = (AST.StmtAssignment (apply s i) (apply s e), p)
    apply s (AST.StmtAssignmentField i f e, p) = (AST.StmtAssignmentField (apply s i) (apply s f) (apply s e), p)
    apply s (AST.StmtFunCall i es, p) = (AST.StmtFunCall (apply s i) (apply s es), p)
    apply s (AST.StmtReturn e, p) = (AST.StmtReturn (apply s e), p)
    apply _ st = st
    applyOnlyRename = undefined

instance Types AST.Field where
    freeTypeVars = undefined
    apply _ f = f
    applyOnlyRename = undefined

instance Types AST.Expression where
    freeTypeVars = undefined
    apply _ e = e
    applyOnlyRename = undefined

instance Types AST.Identifier where
    freeTypeVars = undefined
    apply _ i = i
    applyOnlyRename = undefined
