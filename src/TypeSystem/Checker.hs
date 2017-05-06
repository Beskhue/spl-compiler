{-# OPTIONS_GHC -XFlexibleInstances #-}

{-|
Module: TypeSystem.Checker
Description: A type checker for SPL
Copyright: (c) Thomas Churchman, 2017
License: MIT
Maintainer: thomas@kepow.org
Stability: experimental

A type checker based on Algorithm M.

See more here: http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.65.7733&rep=rep1&type=pdf (last retrieved
10 April 2017).
https://github.com/mgrabmueller/AlgorithmW

-}

module TypeSystem.Checker where

import qualified Debug.Trace as Trace

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Stack as Stack
import qualified Data.Graph as Graph

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
        (res, _) = runTInf $ tInfSPL spl

checkDet :: AST.SPL -> AST.SPL
checkDet spl =
    case check spl of
        Left err -> Trace.trace (show err) undefined
        Right spl -> spl

typeInferenceDet :: (a -> TInf (Substitution, Type)) -> a -> (Substitution, Type)
typeInferenceDet tInf' e =
    case typeInference tInf' e of
        Left err -> Trace.trace (show err) undefined
        Right t -> t

onlyType :: Either String (Substitution, Type) -> Either String Type
onlyType res =
    case res of
        Left err -> Left err
        Right (_, t) -> Right t

typeInference :: (a -> TInf (Substitution, Type)) -> a -> Either String (Substitution, Type)
typeInference tInf' e = res
    where
        (res, _) = runTInf $ tInf' e

typeInferenceExpr :: AST.Expression -> Either String (Substitution, Type)
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

-- |The type inference state consists of the fresh type name generator state and the current type substitution
data TInfState = TInfState { tInfSupply :: Int,
                             tInfSubstitution :: Substitution}
                 deriving (Show)

type TInf a = ExceptT String (ReaderT ScopedTypeCtx (State TInfState)) a

-- |The type inference runner. Results in a tuple of either an error and the type inference result, and the final type
-- inference state. The type inference result will generally be a tuple of a substitution (the type constraints) and the
-- type of an AST branch.
runTInf :: TInf a -> (Either String a, TInfState)
runTInf t = runState (runReaderT (runExceptT t) initTInfCtx) initTInfState
    where
        initTInfCtx = emptyScopedCtx
        initTInfState = TInfState { tInfSupply = 0,
                                    tInfSubstitution = Map.empty}

------------------------------------------------------------------------------------------------------------------------

-- |Generate a new type variable using the type name supplier
newTypeVar :: String -> TInf Type
newTypeVar suffix = do
    s <- get
    put s {tInfSupply = tInfSupply s + 1}
    return (TVar ("t_" ++ show (tInfSupply s) ++ suffix))

substitution :: TInf Substitution
substitution = do
    s <- get
    return $ tInfSubstitution s

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
    | otherwise                     = do
        let s = Map.singleton u t
        st <- get
        put st {tInfSubstitution = s `composeSubstitution` tInfSubstitution st }
        return s

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

getScheme :: String -> TInf Scheme
getScheme varName = do
    ctx <- ask
    s <- substitution
    let ctx' = apply s ctx
    getScheme' (Stack.stackToList ctx') varName
    where
        getScheme' :: [TypeCtx] -> String -> TInf Scheme
        getScheme' [] _ = throwError $ "unbound variable: " ++ varName
        getScheme' (TypeCtx ctx : ctxs) varName =
            case Map.lookup varName ctx of
                Nothing -> getScheme' ctxs varName
                Just scheme -> return scheme

tInfVarName :: String -> TInf Type
tInfVarName varName = do
    ctx <- ask
    s <- substitution
    let ctx' = apply s ctx
    tInfVarName' (Stack.stackToList ctx') varName
    where
        tInfVarName' :: [TypeCtx] -> String -> TInf Type
        tInfVarName' [] _ = throwError $ "unbound variable: " ++ varName
        tInfVarName' (TypeCtx ctx : ctxs) varName =
            case Map.lookup varName ctx of
                Nothing -> tInfVarName' ctxs varName
                Just scheme -> instantiate scheme

-- |Perform type inference on an AST identifier
tInfId :: AST.Identifier -> TInf (Substitution, Type)
tInfId i = do
    t <- tInfVarName (idName i)
    return (nullSubstitution, t)

-- |Perform type inference on an AST constant
tInfConst :: AST.Constant -> TInf (Substitution, Type)
tInfConst (AST.ConstBool _, _) = return (nullSubstitution, TBool)
tInfConst (AST.ConstInt _, _) = return (nullSubstitution, TInt)
tInfConst (AST.ConstChar _, _) = return (nullSubstitution, TChar)
tInfConst (AST.ConstEmptyList, _) = do
    tVar <- newTypeVar "a"
    return (nullSubstitution, TList tVar)

-- |Perform type inference on an AST expression
tInfExpr :: AST.Expression -> TInf (Substitution, Type)
tInfExpr (AST.ExprIdentifier id, _) = tInfId id
tInfExpr (AST.ExprIdentifierField id fields, _) = do
    (s, t) <- tInfId id
    (s', t') <- tTraverseFields t fields
    return (s' `composeSubstitution` s, t')
    where
        tTraverseFields :: Type -> [AST.Field] -> TInf (Substitution, Type)
        tTraverseFields t [] = return (nullSubstitution, t)
        tTraverseFields t (field:fields) =
            case field of
                (AST.FieldHd, _) -> do
                    tVar <- newTypeVar "fld"
                    s <- mgu t (TList tVar)
                    (s', t') <- tTraverseFields (apply s tVar) fields
                    return (s' `composeSubstitution` s, t')
                (AST.FieldTl, _) -> do
                    tVar <- newTypeVar "fld"
                    s <- mgu t (TList tVar)
                    (s', t') <- tTraverseFields (apply s (TList tVar)) fields
                    return (s' `composeSubstitution` s, t')
                (AST.FieldFst, _) -> undefined
                (AST.FieldSnd, _) -> undefined

tInfExpr (AST.ExprFunCall id args, _) = do
    tReturn <- newTypeVar (idName id ++ "_ret")
    (s1, t1) <- tInfId id
    (s2, ts) <- tInfExprs args
    s <- mgu (apply s2 t1) (TFunction ts tReturn)

    return (s2 `composeSubstitution` s1, apply s tReturn)
    --if length args == 0
        --then return (s `composeSubstitution` s2 `composeSubstitution` s1, apply s tReturn)
        --else return (s2 `composeSubstitution` s1, apply s tReturn)
tInfExpr (AST.ExprConstant const, _) = tInfConst const
tInfExpr (AST.ExprTuple e1 e2, _) = tInfTuple e1 e2
tInfExpr (AST.ExprUnaryOp op e, _) = tInfUnaryOp op e
tInfExpr (AST.ExprBinaryOp op e1 e2, _) = tInfBinaryOp op e1 e2

-- |Perform type inference on a list of AST expressions
tInfExprs :: [AST.Expression] -> TInf (Substitution, [Type])
tInfExprs [] = return (nullSubstitution, [])
tInfExprs (expr:exprs) = do
    (s1, t) <- tInfExpr expr
    (s2, ts) <- tInfExprs exprs

    return (s2 `composeSubstitution` s1, apply s2 t : ts)

tInfTuple :: AST.Expression -> AST.Expression -> TInf (Substitution, Type)
tInfTuple e1 e2 = do
    (s1, t1) <- tInfExpr e1
    (s2, t2) <- tInfExpr e2
    return (s2 `composeSubstitution` s1, TTuple (apply s2 t1) t2)

tInfUnaryOp :: AST.UnaryOperator -> AST.Expression -> TInf (Substitution, Type)
tInfUnaryOp (AST.UnaryOpNeg, _) e = do
    (s1, t1) <- tInfExpr e
    s <- mgu t1 TBool
    return (s `composeSubstitution` s1, TBool)
tInfUnaryOp (AST.UnaryOpSubtr, _) e = do
    (s1, t1) <- tInfExpr e
    s <- mgu t1 TInt
    return (s `composeSubstitution` s1, TInt)

tInfBinaryOp :: AST.BinaryOperator -> AST.Expression -> AST.Expression -> TInf (Substitution, Type)
tInfBinaryOp (AST.BinaryOpOr, _) e1 e2 = do
    (s1, t1) <- tInfExpr e1
    s1' <- mgu t1 TBool

    (s2, t2) <- tInfExpr e2
    s2' <- mgu (apply (s2 `composeSubstitution` s1') t1) t2

    return (s2' `composeSubstitution` s2 `composeSubstitution` s1' `composeSubstitution` s1, TBool)
tInfBinaryOp (AST.BinaryOpAnd, p) e1 e2 = tInfBinaryOp (AST.BinaryOpOr, p) e1 e2
tInfBinaryOp (AST.BinaryOpEq, _) e1 e2 = do
    (s1, t1) <- tInfExpr e1
    (s2, t2) <- tInfExpr e2
    s <- mgu (apply s2 t1) t2
    return (s `composeSubstitution` s2 `composeSubstitution` s1, TBool)
tInfBinaryOp (AST.BinaryOpNEq, p) e1 e2 = tInfBinaryOp (AST.BinaryOpEq, p) e1 e2
tInfBinaryOp (AST.BinaryOpLT, _) e1 e2 = do
    (s1, t1) <- tInfExpr e1
    s1' <- mgu t1 TInt

    (s2, t2) <- tInfExpr e2
    s2' <- mgu (apply (s2 `composeSubstitution` s1') t1) t2

    return (s2' `composeSubstitution` s2 `composeSubstitution` s1' `composeSubstitution` s1, TBool)
tInfBinaryOp (AST.BinaryOpGT, p) e1 e2 = tInfBinaryOp (AST.BinaryOpLT, p) e1 e2
tInfBinaryOp (AST.BinaryOpLTE, p) e1 e2= tInfBinaryOp (AST.BinaryOpLT, p) e1 e2
tInfBinaryOp (AST.BinaryOpGTE, p) e1 e2 = tInfBinaryOp (AST.BinaryOpLT, p) e1 e2
tInfBinaryOp (AST.BinaryOpConcat, _) e1 e2 = do
    (s1, t1) <- tInfExpr e1
    (s2, t2) <- tInfExpr e2
    s <- mgu (TList (apply s2 t1)) t2
    return (s `composeSubstitution` s2 `composeSubstitution` s1, apply s t2)
tInfBinaryOp (AST.BinaryOpPlus, _) e1 e2 = do
    (s1, t1) <- tInfExpr e1
    s1' <- mgu t1 TInt

    (s2, t2) <- tInfExpr e2
    s2' <- mgu (apply (s2 `composeSubstitution` s1') t1) t2

    return (s2' `composeSubstitution` s2 `composeSubstitution` s1' `composeSubstitution` s1, TInt)
tInfBinaryOp (AST.BinaryOpSubtr, p) e1 e2 = tInfBinaryOp (AST.BinaryOpPlus, p) e1 e2
tInfBinaryOp (AST.BinaryOpMult, p) e1 e2 = tInfBinaryOp (AST.BinaryOpPlus, p) e1 e2
tInfBinaryOp (AST.BinaryOpDiv, p) e1 e2 = tInfBinaryOp (AST.BinaryOpPlus, p) e1 e2
tInfBinaryOp (AST.BinaryOpMod, p) e1 e2 = tInfBinaryOp (AST.BinaryOpPlus, p) e1 e2

------------------------------------------------------------------------------------------------------------------------

-- |Perform type inference on the global SPL declarations. Rewrite the AST such that all variables are typed as
-- completely as possible.
tInfSPL :: AST.SPL -> TInf (AST.SPL, Substitution, Type)
tInfSPL decls = do
    ctx <- ask
    let graph = tInfSPLGraph decls
    throwError $ show graph
    let initCtx = Stack.stackPush ctx emptyCtx -- Create top scope
    ctx' <- addGlobalsToCtx initCtx decls -- Add globals to to scope
    local (\ _ -> ctx') (tInfSPL' decls)

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

        tInfSPL' :: AST.SPL -> TInf (AST.SPL, Substitution, Type)
        tInfSPL' [] = return ([], nullSubstitution, TVoid)
        tInfSPL' (decl:decls) = do
            -- Infer type of the declaration
            (decl', s1, varName, t1) <- tInfDecl decl

            -- Get the type scheme of the variable/function we just declared and unify it with the actual type
            (Scheme _ t1') <- getScheme varName
            s2 <- mgu t1' t1

            -- Get the next declarations, using the unified type scheme
            (decls', s3, t2) <- tInfSPL' decls

            -- Infer type for this declaration again, using the "back-flown" information from the next declarations
            --(declPost, s1Post, varNamePost, t1Post) <- tInfDecl (apply s3 ctx) decl

            return (
                        apply (s3 `composeSubstitution` s2 ) decl' : decls',
                        s3 `composeSubstitution` s2 `composeSubstitution` s1,
                        t2
                    )

-- |Find the graph of (global) dependencies; a list of tuples of declarations, identifiers of those declarations,
-- and the (global) identifiers those declarations depend on
tInfSPLGraph :: AST.SPL -> [(AST.Decl, String, [String])]
tInfSPLGraph decls =
    let globalVars = map declIdentifier decls in
        map (\decl -> (decl, declIdentifier decl, snd $ dependencies globalVars decl)) decls

tInfDecl :: AST.Decl -> TInf (AST.Decl, Substitution, String, Type)
tInfDecl (AST.DeclV decl, p) = do
    (decl', s, varName, t) <- tInfVarDecl decl
    return ((AST.DeclV decl', p), s, varName, t)
tInfDecl (AST.DeclF decl, p) = do
    (decl', s, varName, t) <- tInfFunDecl decl
    return ((AST.DeclF decl', p), s, varName, t)

tInfVarDecl :: AST.VarDecl -> TInf (AST.VarDecl, Substitution, String, Type)
tInfVarDecl decl =
    case decl of
        (AST.VarDeclTyped annotatedType identifier expr, p) ->
            let annotatedT = rTranslateType annotatedType in do
            (ast, s, str, t) <- tInfVarDecl' p identifier expr
            s' <- mgu annotatedT t `catchError` (\_ ->
                throwError $ "Could not unify types"
                    ++ ". Expected type: " ++ AST.prettyPrint (translateType p annotatedT)
                    ++ ". Inferred type: " ++ AST.prettyPrint (translateType p t) ++ ".")
            if apply s' annotatedT == applyOnlyRename s' annotatedT
                then return (apply s' ast, s' `composeSubstitution` s, str, apply s' t)
                else throwError $ "Expected type is more general than the inferred type"
                    ++ ". Expected type: " ++ AST.prettyPrint (translateType p annotatedT)
                    ++ ". Inferred type: " ++ AST.prettyPrint (translateType p t) ++ "."
        (AST.VarDeclUntyped identifier expr, p) -> tInfVarDecl' p identifier expr
    where
        tInfVarDecl' :: Pos.Pos -> AST.Identifier -> AST.Expression -> TInf (AST.VarDecl, Substitution, String, Type)
        tInfVarDecl' p identifier expr = do
            (s, t) <- tInfExpr expr
            let t' = translateType p t
            return ((AST.VarDeclTyped t' identifier expr, p), s, idName identifier, t)

tInfFunDecl :: AST.FunDecl -> TInf (AST.FunDecl, Substitution, String, Type)
tInfFunDecl decl =
    case decl of
        (AST.FunDeclTyped identifier args annotatedType stmts, p) ->
            let annotatedT = rTranslateFunType annotatedType in do
            (ast, s, str, t) <- tInfFunDecl' p identifier args stmts
            s' <- mgu annotatedT t `catchError` (\_ ->
                throwError $ "Could not unify types"
                    ++ ". Expected type: " ++ AST.prettyPrint (translateFunType p annotatedT)
                    ++ ". Inferred type: " ++ AST.prettyPrint (translateFunType p t) ++ ".")
            if apply s' annotatedT == applyOnlyRename s' annotatedT
                then return (apply s' ast, s' `composeSubstitution` s, str, apply s' t)
                else throwError $ "Expected type is more general than the inferred type"
                    ++ ". Expected type: " ++ AST.prettyPrint (translateFunType p annotatedT)
                    ++ ". Inferred type: " ++ AST.prettyPrint (translateFunType p t) ++ "."
        (AST.FunDeclUntyped identifier args stmts, p) -> tInfFunDecl' p identifier args stmts
    where
        tInfFunDecl' :: Pos.Pos -> AST.Identifier -> [AST.Identifier] -> [AST.Statement] -> TInf (AST.FunDecl, Substitution, String, Type)
        tInfFunDecl' p identifier args stmts = do
            ctx <- ask
            let newCtx = Stack.stackPush ctx emptyCtx
            scopedCtx <- addArgsToCtx (idName identifier ++ "_") newCtx args -- Create the function's scoped context
            local (\_ -> scopedCtx) (do
                (stmts', s, t) <- tInfStatements stmts

                argsTypes <- getArgsTypes args
                let funType = TFunction argsTypes t
                let funTypeAST = translateFunType p funType

                return ((AST.FunDeclTyped identifier args funTypeAST stmts', p), s, idName identifier, funType))
            where
                addArgsToCtx :: String -> ScopedTypeCtx -> [AST.Identifier] -> TInf ScopedTypeCtx
                addArgsToCtx prefix ctx [] = return ctx
                addArgsToCtx prefix ctx (arg:args) = do
                    typeVar <- newTypeVar (prefix ++ "arg")
                    ctx' <- addArgsToCtx prefix ctx args
                    return $ add ctx' (idName arg) (Scheme [] typeVar)
                    -- return $ add ctx' (idName arg) (generalize ctx' typeVar)

                getArgsTypes :: [AST.Identifier] -> TInf [Type]
                getArgsTypes [] = return []
                getArgsTypes (arg:args) = do
                    t <- tInfVarName $ idName arg
                    ts <- getArgsTypes args
                    return $ t:ts


tInfStatements :: [AST.Statement] -> TInf ([AST.Statement], Substitution, Type)
tInfStatements [] = return ([], nullSubstitution, TVoid)
tInfStatements (statement:statements) = do
    (statement', s1, varName, t1, returnsValue) <- tInfStatement statement

    -- Update local context if the statement declared a new variable
    ctx <- ask
    let ctx' = (if varName == ""
                    then ctx
                    else add ctx varName (Scheme [] t1)
                )
    local (\_ -> ctx') (do
        (statements', s2, t2) <- tInfStatements statements

        if returnsValue
            then
                case t2 of
                    TVoid -> return (apply s2 statement' : statements', s2 `composeSubstitution` s1, apply s2 t1)
                    _ -> do
                        s <- mgu (apply s2 t1) t2
                        return (
                            apply (s `composeSubstitution` s2) statement' : statements',
                            s `composeSubstitution` s2 `composeSubstitution` s1,
                            apply s t2)
            else return (apply s2 statement' : statements', s2 `composeSubstitution` s1, t2))

tInfStatement :: AST.Statement -> TInf (AST.Statement, Substitution, String, Type, Bool)
tInfStatement (AST.StmtVarDecl decl, p) = do
    (decl', s, varName, t) <- tInfVarDecl decl
    ctx <- ask
    case Stack.stackPeek ctx of
        Just (TypeCtx ctx') ->
            if Map.member varName ctx'
                then throwError $ "Variable multiply defined in scope: " ++ varName ++ show p
                else return ((AST.StmtVarDecl decl', p), s, varName, t, False)

tInfStatement (AST.StmtIf expr st, p) = do
    (s1, t1) <- tInfExpr expr
    s <- mgu t1 TBool
    (st', s2, varName, t2, returnsValue) <- tInfStatement st
    return ((AST.StmtIf expr st', p), s2 `composeSubstitution` s `composeSubstitution` s1, varName, t2, returnsValue)
tInfStatement (AST.StmtIfElse expr st1 st2, p) = do
    (es, et1) <- tInfExpr expr
    s <- mgu et1 TBool
    (st1', s1', varName, st1, returnsValue1) <- tInfStatement st1
    (st2', s2', varName, st2, returnsValue2) <- tInfStatement st2

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
tInfStatement (AST.StmtWhile expr st, p) = do
    (s1, t1) <- tInfExpr expr
    s <- mgu t1 TBool
    (st', s2, varName, t2, returnsValue) <- tInfStatement st
    return ((AST.StmtWhile expr st', p), s2 `composeSubstitution` s `composeSubstitution` s1, varName, t2, returnsValue)
tInfStatement (AST.StmtBlock stmts, p) = do
    ctx <- ask
    let newCtx = Stack.stackPush ctx emptyCtx
    local (\_ -> newCtx) (do
        (stmts, s, t) <- tInfStatements stmts
        case t of
            TVoid -> return ((AST.StmtBlock stmts, p), s, "", t, False)
            _ -> return ((AST.StmtBlock stmts, p), s, "", t, True))
tInfStatement (AST.StmtAssignment identifier expr, p) = do
    (Scheme _ t) <- getScheme (idName identifier)
    (s1, t1) <- tInfExpr expr

    s <- mgu t t1

    let t' = translateType p (apply s t1)
    return ((AST.StmtAssignment identifier expr, p), s `composeSubstitution` s1, "", apply s t1, False)
tInfStatement (AST.StmtAssignmentField identifier fields expr, p) = throwError "Assigning to fields is not supported"
tInfStatement (AST.StmtFunCall identifier expressions, p) = do
    (s, t) <- tInfExpr (AST.ExprFunCall identifier expressions, p)
    return ((AST.StmtFunCall identifier expressions, p), s, "", TVoid, False)
tInfStatement (AST.StmtReturn expr, p) = do
    (s, t) <- tInfExpr expr
    return ((AST.StmtReturn expr, p), s, "", t, True)
tInfStatement (AST.StmtReturnVoid, p) = return ((AST.StmtReturnVoid, p), nullSubstitution, "", TVoid, True)

------------------------------------------------------------------------------------------------------------------------

class DeclIdentifier a where
    declIdentifier :: a -> String

instance DeclIdentifier AST.Decl where
    declIdentifier (AST.DeclV decl, _) = declIdentifier decl
    declIdentifier (AST.DeclF decl, _) = declIdentifier decl

instance DeclIdentifier AST.VarDecl where
    declIdentifier (AST.VarDeclTyped _ i _, _) = idName i
    declIdentifier (AST.VarDeclUntyped i _, _) = idName i

instance DeclIdentifier AST.FunDecl where
    declIdentifier (AST.FunDeclTyped i _ _ _, _) = idName i
    declIdentifier (AST.FunDeclUntyped i _ _, _) = idName i

class Dependencies a where
    dependencies :: [String] -> a -> ([String], [String]) -- tuple of global defs remaining and dependencies

instance Dependencies a => Dependencies [a] where
    dependencies globalDefs [] = (globalDefs, [])
    dependencies globalDefs (a:aa) =
        let (globalDefs', deps) = dependencies globalDefs a in
            let (globalDefs'', deps') = dependencies globalDefs' aa in
                (globalDefs'', deps ++ deps')

instance Dependencies AST.Decl where
    dependencies globalDefs (AST.DeclV decl, _) = dependencies globalDefs decl
    dependencies globalDefs (AST.DeclF decl, _) = dependencies globalDefs decl

instance Dependencies AST.VarDecl where
    dependencies globalDefs (AST.VarDeclTyped _ i e, _) =
        let (globalDefs', deps) = dependencies globalDefs e in
          ([g | g <- globalDefs', g /= idName i], deps)
    dependencies globalDefs (AST.VarDeclUntyped i e, _) =
        let (globalDefs', deps) = dependencies globalDefs e in
          ([g | g <- globalDefs, g /= idName i], deps)

instance Dependencies AST.FunDecl where
    dependencies globalDefs (AST.FunDeclTyped i is _ ss, _) =
        let (globalDefs', deps) = dependencies [g | g <- globalDefs, g `notElem` map idName is] ss in
            ([g | g <- globalDefs', g /= idName i], deps)
    dependencies globalDefs (AST.FunDeclUntyped i is ss, _) =
        let (globalDefs', deps) = dependencies [g | g <- globalDefs, g `notElem` map idName is] ss in
            ([g | g <- globalDefs', g /= idName i], deps)

instance Dependencies AST.Statement where
    dependencies globalDefs _ = (globalDefs, globalDefs)

instance Dependencies AST.Expression where
    dependencies globalDefs _ = (globalDefs, globalDefs)

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
