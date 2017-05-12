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
import qualified Data.Graph.SCC as Graph.SCC
import Data.List (elemIndex)

import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State

import qualified Data.Pos as Pos
import qualified Data.AST as AST

------------------------------------------------------------------------------------------------------------------------

check :: Bool -> AST.SPL -> Either String (AST.SPL, ASTAnnotation)
check preserveDeclOrder spl = res
    where
        (res, _) = runTInf $ tInfSPL preserveDeclOrder spl

checkDet :: Bool -> AST.SPL -> (AST.SPL, ASTAnnotation)
checkDet preserveDeclOrder spl =
    case check preserveDeclOrder spl of
        Left err -> Trace.trace (show err) undefined
        Right spl' -> spl'

------------------------------------------------------------------------------------------------------------------------

typeInferenceExpr :: (AST.Expression -> TInf Type) -> AST.Expression -> Either String Type
typeInferenceExpr tInf' expr = res
    where
        (res, _) = runTInf $ tInf' expr

------------------------------------------------------------------------------------------------------------------------

-- |The possible types
data Type = TVar String
          | TBool
          | TInt
          | TChar
          | TList Type
          | TTuple Type Type
          | TFunction [Type] Type
          | TPointer Type
          | TVoid
            deriving (Show, Eq, Ord)

-- |A type scheme (polytype): a type with a list of bound type variables (the type variables not bound are still free)
data Scheme = Scheme [String] Type
              deriving (Show)

------------------------------------------------------------------------------------------------------------------------

data ValueType = PersistentValue
               | TemporaryValue
                 deriving (Show, Eq)

valueType :: AST.Expression -> ValueType
valueType (AST.ExprIdentifier _, _) = PersistentValue
valueType (AST.ExprIdentifierField _ _, _) = PersistentValue
valueType (AST.ExprUnaryOp (AST.UnaryOpDereference, _) e, _) = valueType e
valueType _ = TemporaryValue

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
    freeTypeVars (TPointer t) = freeTypeVars t
    freeTypeVars TVoid = Set.empty

    apply s (TVar v) =
        case Map.lookup v s of
            Just t -> t
            _ -> TVar v
    apply s (TList l) = TList $ apply s l
    apply s (TTuple t1 t2) = TTuple (apply s t1) (apply s t2)
    apply s (TFunction arg body) = TFunction (apply s arg) (apply s body)
    apply s (TPointer t) = TPointer $ apply s t
    apply s t = t

    applyOnlyRename s (TVar v) =
        case Map.lookup v s of
            Just (TVar v') -> TVar v'
            _ -> TVar v
    applyOnlyRename s (TList l) = TList $ applyOnlyRename s l
    applyOnlyRename s (TTuple t1 t2) = TTuple (applyOnlyRename s t1) (applyOnlyRename s t2)
    applyOnlyRename s (TFunction arg body) = TFunction (applyOnlyRename s arg) (applyOnlyRename s body)
    applyOnlyRename s (TPointer t) = TPointer $ applyOnlyRename s t
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
composeSubstitution s1 s2 = Map.map (apply s1) s2 `Map.union` s1

------------------------------------------------------------------------------------------------------------------------

-- |The AST annotation is a (finite) mapping from source code positions (corresponding with entities at those positions)
-- to their types
type ASTAnnotation = Map.Map Pos.Pos Type

emptyAnnotation :: ASTAnnotation
emptyAnnotation = Map.empty

instance Types ASTAnnotation where
    freeTypeVars = undefined
    apply s m = Map.map (apply s) m
    applyOnlyRename = undefined

------------------------------------------------------------------------------------------------------------------------

-- |A type context (or environment) is a mapping from term variables to type schemes
newtype TypeCtx = TypeCtx (Map.Map String Scheme)
                  deriving (Show)

type ScopedTypeCtx = Stack.Stack TypeCtx

emptyMap :: Map.Map String Scheme
emptyMap = Map.empty

emptyCtx :: TypeCtx
emptyCtx = TypeCtx emptyMap

builtInCtx :: TypeCtx
builtInCtx = TypeCtx $ Map.fromList [
    ("print", let t = TVar "a" in Scheme ["a"] (TFunction [t] TBool)),
    ("isEmpty", let t = TList (TVar "a") in Scheme ["a"] (TFunction [t] TBool)),
    ("length", let t = TList (TVar "a") in Scheme ["a"] (TFunction [t] TInt))
    ]

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
                             tInfSubstitution :: Substitution,
                             astAnnotation :: ASTAnnotation}
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
                                    tInfSubstitution = Map.empty,
                                    astAnnotation = Map.empty}

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

substitute :: Type -> TInf Type
substitute t = do
    s <- substitution
    return $ apply s t

-- |Replace bound type variables in a scheme with fresh type variables
instantiate :: Scheme -> TInf Type
instantiate (Scheme vars t) = do
    newVars <- mapM (\_ -> newTypeVar "a") vars
    let s = Map.fromList (zip vars newVars)
    return $ apply s t

------------------------------------------------------------------------------------------------------------------------

annotate :: Pos.Pos -> Type -> TInf ()
annotate p t = do
    s <- get
    put s {astAnnotation = Map.insert p t (astAnnotation s) }

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
mgu :: Maybe Pos.Pos -> Type -> Type -> TInf Substitution
mgu p t1 t2 = do
    t1' <- substitute t1
    t2' <- substitute t2
    s <- mgu' t1' t2'
    case p of
        Nothing -> return s
        Just p' -> do
            t <- substitute t1
            annotate p' t
            return s
    where
    mgu' :: Type -> Type -> TInf Substitution
    mgu' (TVar u) t                 = varBind u t
    mgu' t (TVar u)                 = varBind u t
    mgu' TBool TBool                = return nullSubstitution
    mgu' TInt TInt                  = return nullSubstitution
    mgu' TChar TChar                = return nullSubstitution
    mgu' (TList t) (TList t')       = mgu' t t'
    mgu' (TTuple t1 t2) (TTuple t1' t2') = do
        s1 <- mgu' t1 t1'
        s2 <- mgu' (apply s1 t2) (apply s1 t2')
        return $ s1 `composeSubstitution` s2
    mgu' (TFunction [] body) (TFunction [] body') = mgu' body body'
    mgu' (TFunction (arg:args) body) (TFunction (arg':args') body') = do
        s1 <- mgu' arg arg'
        s2 <- mgu' (apply s1 (TFunction args body)) (apply s1 (TFunction args' body'))
        return $ s2 `composeSubstitution` s1
    mgu' (TPointer t) (TPointer t') = mgu' t t'
    mgu' TVoid TVoid                = return nullSubstitution
    mgu' t1 t2                      = throwError $ "types do not unify: " ++ show t1 ++ " and " ++ show t2

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
        tInfVarName' [] varName = throwError $ "unbound variable: " ++ varName
        tInfVarName' (TypeCtx ctx : ctxs) varName =
            case Map.lookup varName ctx of
                Nothing -> tInfVarName' ctxs varName
                Just scheme -> instantiate scheme

-- |Perform type inference on an AST identifier
tInfId :: AST.Identifier -> TInf Type
tInfId i = tInfVarName (idName i)

-- |Perform type inference on an AST constant
tInfConst :: Type -> AST.Constant -> TInf ()
tInfConst t (AST.ConstBool _, p) = void $ mgu (Just p) t TBool
tInfConst t (AST.ConstInt _, p) = void $ mgu (Just p) t TInt
tInfConst t (AST.ConstChar _, p) = void $ mgu (Just p) t TChar
tInfConst t (AST.ConstEmptyList, p) = do
    tVar <- newTypeVar "a"
    void $ mgu (Just p) t (TList tVar)


tInfExprTyped :: AST.Expression -> TInf Type
tInfExprTyped e = do
    t <- newTypeVar "t"
    tInfExpr t e
    substitute t

-- |Perform type inference on an AST expression
tInfExpr :: Type -> AST.Expression -> TInf ()
tInfExpr t (AST.ExprIdentifier id, p) = do
    t' <- tInfId id
    void $ mgu (Just p) t t'
tInfExpr t (AST.ExprIdentifierField id fields, _) = do
    t' <- tInfId id
    tTraverseFields t t' fields
tInfExpr t (AST.ExprFunCall id args, p) = do
    t1 <- tInfId id
    ts <- mapM (const $ newTypeVar "arg") args
    mgu (Just p) (TFunction ts t) t1
    tInfExprs ts args
tInfExpr t (AST.ExprConstant const, _) = tInfConst t const
tInfExpr t (AST.ExprTuple e1 e2, p) = do
    t1 <- newTypeVar "tuple"
    t2 <- newTypeVar "tuple"
    tInfExpr t1 e1
    tInfExpr t2 e2
    void $ mgu (Just p) t (TTuple t1 t2)
tInfExpr t (AST.ExprUnaryOp op e, p) = do
    tInfUnaryOp t op e
    t' <- substitute t
    annotate p t'
tInfExpr t (AST.ExprBinaryOp op e1 e2, p) = do
    tInfBinaryOp t op e1 e2
    t' <- substitute t
    annotate p t'

tTraverseFields :: Type -> Type -> [AST.Field] -> TInf ()
tTraverseFields t t' [] = void $ mgu Nothing t t'
tTraverseFields t t' (field:fields) =
    case field of
        (AST.FieldHd, p) -> do
            tVar <- newTypeVar "fld"
            s <- mgu (Just p) t' (TList tVar)
            tTraverseFields t (apply s tVar) fields
        (AST.FieldTl, p) -> do
            tVar <- newTypeVar "fld"
            s <- mgu (Just p) t' (TList tVar)
            tTraverseFields t (apply s (TList tVar)) fields
        (AST.FieldFst, p) -> do
            tVar1 <- newTypeVar "fld"
            tVar2 <- newTypeVar "fld"
            s <- mgu (Just p) t' (TTuple tVar1 tVar2)
            tTraverseFields t (apply s tVar1) fields
        (AST.FieldSnd, p) -> do
            tVar1 <- newTypeVar "fld"
            tVar2 <- newTypeVar "fld"
            s <- mgu (Just p) t' (TTuple tVar1 tVar2)
            tTraverseFields t (apply s tVar2) fields

-- |Perform type inference on a list of AST expressions
tInfExprs :: [Type] -> [AST.Expression] -> TInf ()
tInfExprs [] [] = void $ mgu Nothing TBool TBool -- todo clean this up
tInfExprs (t:ts) (expr:exprs) = do
    tInfExpr t expr
    tInfExprs ts exprs

tInfUnaryOp :: Type -> AST.UnaryOperator -> AST.Expression -> TInf ()
tInfUnaryOp t (AST.UnaryOpNeg, p) e = do
    tInfExpr TBool e
    void $ mgu (Just p) t TBool
tInfUnaryOp t (AST.UnaryOpBitwiseNot, p) e = do
    tInfExpr TInt e
    void $ mgu (Just p) t TInt
tInfUnaryOp t (AST.UnaryOpSubtr, p) e = do
    tInfExpr TInt e
    void $ mgu (Just p) t TInt
tInfUnaryOp t (AST.UnaryOpCast castToType, p) e = do
    t' <- newTypeVar "var"
    tInfExpr t' e
    void $ mgu (Just p) t (rTranslateType castToType)
tInfUnaryOp t (AST.UnaryOpReference, p) e =
    case valueType e of
        PersistentValue -> do
            t' <- newTypeVar "ref"
            tInfExpr t' e
            void $ mgu (Just p) t (TPointer t')
        _ -> throwError $ "persistent value required for '&' operand"
tInfUnaryOp t (AST.UnaryOpDereference, p) e = tInfExpr (TPointer t) e

tInfBinaryOp :: Type -> AST.BinaryOperator -> AST.Expression -> AST.Expression -> TInf ()
tInfBinaryOp t (AST.BinaryOpOr, p) e1 e2 = do
    tInfExpr TBool e1
    tInfExpr TBool e2
    void $ mgu (Just p) t TBool
tInfBinaryOp t (AST.BinaryOpBitwiseOr, p) e1 e2 = do
    tInfExpr TInt e1
    tInfExpr TInt e2
    void $ mgu (Just p) t TInt
tInfBinaryOp t (AST.BinaryOpAnd, p) e1 e2 = tInfBinaryOp t (AST.BinaryOpOr, p) e1 e2
tInfBinaryOp t (AST.BinaryOpBitwiseAnd, p) e1 e2 = tInfBinaryOp t (AST.BinaryOpBitwiseOr, p) e1 e2
tInfBinaryOp t (AST.BinaryOpEq, p) e1 e2 = do
    t' <- newTypeVar "expr"
    tInfExpr t' e1
    tInfExpr t' e2
    void $ mgu (Just p) t TBool
tInfBinaryOp t (AST.BinaryOpNEq, p) e1 e2 = tInfBinaryOp t (AST.BinaryOpEq, p) e1 e2
tInfBinaryOp t (AST.BinaryOpLT, p) e1 e2 = do
    tInfExpr TInt e1
    tInfExpr TInt e2
    void $ mgu (Just p) t TBool
tInfBinaryOp t (AST.BinaryOpGT, p) e1 e2 = tInfBinaryOp t (AST.BinaryOpLT, p) e1 e2
tInfBinaryOp t (AST.BinaryOpLTE, p) e1 e2= tInfBinaryOp t (AST.BinaryOpLT, p) e1 e2
tInfBinaryOp t (AST.BinaryOpGTE, p) e1 e2 = tInfBinaryOp t (AST.BinaryOpLT, p) e1 e2
tInfBinaryOp t (AST.BinaryOpConcat, p) e1 e2 = do
    t' <- newTypeVar "a"
    tInfExpr t' e1
    tInfExpr (TList t') e2
    void $ mgu (Just p) t (TList t')
tInfBinaryOp t (AST.BinaryOpPlus, p) e1 e2 = do
    tInfExpr TInt e1
    tInfExpr TInt e2
    void $ mgu (Just p) t TInt
tInfBinaryOp t (AST.BinaryOpSubtr, p) e1 e2 = tInfBinaryOp t (AST.BinaryOpPlus, p) e1 e2
tInfBinaryOp t (AST.BinaryOpMult, p) e1 e2 = tInfBinaryOp t (AST.BinaryOpPlus, p) e1 e2
tInfBinaryOp t (AST.BinaryOpDiv, p) e1 e2 = tInfBinaryOp t (AST.BinaryOpPlus, p) e1 e2
tInfBinaryOp t (AST.BinaryOpMod, p) e1 e2 = tInfBinaryOp t (AST.BinaryOpPlus, p) e1 e2

------------------------------------------------------------------------------------------------------------------------

-- |Perform type inference on the global SPL declarations. Rewrite the AST such that all variables are typed as
-- completely as possible.
tInfSPL :: Bool -> AST.SPL -> TInf (AST.SPL, ASTAnnotation)
tInfSPL preserveDeclOrder decls = do
    ctx <- ask
    let deps = tInfSPLGraph decls
    let (graph, vertexToEdge, keyToVertex) = Graph.graphFromEdges deps
    let (sccTopo, _) = Graph.SCC.scc graph -- Calculate strongly connected components
    let scc = reverse sccTopo -- Strongly connected components in reverse topological order ([(sccId, [keys])])
    -- Calculate list of strongly connected declarations and, if declration order is to be preserved, the original location of the declarations
    -- [[(originalIndex, decl)]]
    let sccDecls = if preserveDeclOrder
        then map (map (\vertex -> (\d@(decl, _, _) -> (case elemIndex d deps of Just idx -> idx, decl)) $ vertexToEdge vertex) . snd) scc
        else numberAscending $ map (map (\vertex -> let d@(decl, _, _) = vertexToEdge vertex in decl) . snd) scc
    let initCtx = Stack.stackPush ctx builtInCtx -- Create top scope
    spl <- local (const initCtx) (tInfSCCs decls sccDecls)
    s <- substitution
    st <- get
    return (spl, apply s (astAnnotation st))
    where
        numberAscending :: [[AST.Decl]] -> [[(Int, AST.Decl)]]
        numberAscending = numberAscending' 0
            where
                numberAscending' :: Int -> [[AST.Decl]] -> [[(Int, AST.Decl)]]
                numberAscending' _ [] = []
                numberAscending' n (decls:sscs) = zip [n..] decls : numberAscending' (n + length decls) sscs
        addGlobalsToCtx :: ScopedTypeCtx -> AST.SPL -> TInf ScopedTypeCtx
        addGlobalsToCtx ctx [] = return ctx
        addGlobalsToCtx ctx (decl:decls) = do
            typeVar <- newTypeVar "global" -- Create a (temporary) new type var for this global
            ctx' <- addGlobalsToCtx ctx decls
            case decl of
                (AST.DeclV (AST.VarDeclTyped _ i _, _), _) -> return $ add ctx' (idName i) (Scheme [] typeVar)
                (AST.DeclV (AST.VarDeclUntyped i _, _), _) -> return $ add ctx' (idName i) (Scheme [] typeVar)
                (AST.DeclF (AST.FunDeclTyped i _ _ _, _), _) -> return $ add ctx' (idName i) (Scheme [] typeVar)
                (AST.DeclF (AST.FunDeclUntyped i _ _, _), _) -> return $ add ctx' (idName i) (Scheme [] typeVar)
        addGlobalToCtx :: ScopedTypeCtx -> AST.Decl -> Type -> TInf ScopedTypeCtx
        addGlobalToCtx ctx decl t =
            case decl of
                (AST.DeclV (AST.VarDeclTyped _ i _, _), _) -> return $ add ctx (idName i) (Scheme [] t)
                (AST.DeclV (AST.VarDeclUntyped i _, _), _) -> return $ add ctx (idName i) (Scheme [] t)
                (AST.DeclF (AST.FunDeclTyped i _ _ _, _), _) -> return $ add ctx (idName i) (generalize ctx t)
                (AST.DeclF (AST.FunDeclUntyped i _ _, _), _) -> return $ add ctx (idName i) (generalize ctx t)
        insertIntoSPL :: AST.SPL -> Int -> AST.Decl -> AST.SPL
        insertIntoSPL spl idx decl =
            let (start, _:end) = splitAt idx spl in
                start ++ decl : end
        -- |Perform type inference for each strongly connected component
        tInfSCCs :: AST.SPL -> [[(Int, AST.Decl)]] -> TInf AST.SPL
        tInfSCCs spl [] = return spl
        tInfSCCs spl (scc:sccs) = do
            ctx <- ask
            let decls = map snd scc
            ctx' <- addGlobalsToCtx ctx decls
            typedDecls <- local (const ctx') (tInfSCC scc)
            (spl', ctx'') <- finalizeSCC spl ctx typedDecls
            local (const ctx'') (tInfSCCs spl' sccs)
        -- |Perform type inference for declaration within a strongly connected component
        tInfSCC :: [(Int, AST.Decl)] -> TInf [(Int, AST.Decl, Type)]
        tInfSCC [] = return []
        tInfSCC ((idx, decl):decls) = do
            -- Infer type of the declaration
            t1 <- newTypeVar "decl"
            (decl', varName) <- tInfDecl t1 decl

            -- Get the type scheme of the variable/function we just declared and unify it with the actual type
            (Scheme _ t1') <- getScheme varName
            mgu Nothing t1' t1

            -- Perform type inference for the next declarations
            typedDecls <- tInfSCC decls

            s <- substitution
            return $ (idx, rewrite s decl', apply s t1) : typedDecls
        -- |Add the types of the global declarations in the SCC to the context,
        -- and update the AST (generalizes the types of function declaration).
        finalizeSCC :: AST.SPL -> ScopedTypeCtx -> [(Int, AST.Decl, Type)] -> TInf (AST.SPL, ScopedTypeCtx)
        finalizeSCC spl ctx [] = return (spl, ctx)
        finalizeSCC spl ctx ((idx, decl, t):decls) = do
            s <- substitution
            ctx' <- addGlobalToCtx ctx decl t
            let spl' = insertIntoSPL spl idx decl
            finalizeSCC spl' ctx' decls

-- |Find the graph of (global) dependencies; a list of tuples of declarations, identifiers of those declarations,
-- and the (global) identifiers those declarations depend on
tInfSPLGraph :: AST.SPL -> [(AST.Decl, String, [String])]
tInfSPLGraph decls =
    let globalVars = map declIdentifier decls in
        map (\decl -> (decl, declIdentifier decl, snd $ dependencies globalVars decl)) decls

tInfDecl :: Type -> AST.Decl -> TInf (AST.Decl, String)
tInfDecl t (AST.DeclV decl, p) = do
    (decl', varName) <- tInfVarDecl t decl
    return ((AST.DeclV decl', p), varName)
tInfDecl t (AST.DeclF decl, p) = do
    (decl', varName) <- tInfFunDecl t decl
    return ((AST.DeclF decl', p), varName)

tInfVarDecl :: Type -> AST.VarDecl -> TInf (AST.VarDecl, String)
tInfVarDecl t decl =
    case decl of
        (AST.VarDeclTyped annotatedType identifier expr, p) -> do
            let annotatedT = rTranslateType annotatedType
            (ast, str) <- tInfVarDecl' p t identifier expr
            t' <- substitute t
            s' <- mgu (Just p) annotatedT t' `catchError` (\_ ->
                throwError $ "Could not unify types"
                    ++ ". Expected type: " ++ AST.prettyPrint (translateType p annotatedT)
                    ++ ". Inferred type: " ++ AST.prettyPrint (translateType p t') ++ ".")
            if apply s' annotatedT == applyOnlyRename s' annotatedT
                then return (rewrite s' ast, str)
                else throwError $ "Expected type is more general than the inferred type"
                    ++ ". Expected type: " ++ AST.prettyPrint (translateType p annotatedT)
                    ++ ". Inferred type: " ++ AST.prettyPrint (translateType p t') ++ "."
        (AST.VarDeclUntyped identifier expr, p) -> tInfVarDecl' p t identifier expr
    where
        tInfVarDecl' :: Pos.Pos -> Type -> AST.Identifier -> AST.Expression -> TInf (AST.VarDecl, String)
        tInfVarDecl' p t identifier expr = do
            tInfExpr t expr
            t' <- substitute t
            let t'' = translateType p t'
            return ((AST.VarDeclTyped t'' identifier expr, p), idName identifier)

tInfFunDecl :: Type -> AST.FunDecl -> TInf (AST.FunDecl, String)
tInfFunDecl t decl =
    case decl of
        (AST.FunDeclTyped identifier args annotatedType stmts, p) -> do
            let annotatedT = rTranslateFunType annotatedType
            (ast, str) <- tInfFunDecl' p t identifier args stmts
            t' <- substitute t
            s' <- mgu (Just p) annotatedT t `catchError` (\_ ->
                throwError $ "Could not unify types"
                    ++ ". Expected type: " ++ AST.prettyPrint (translateFunType p annotatedT)
                    ++ ". Inferred type: " ++ AST.prettyPrint (translateFunType p t') ++ ".")
            if apply s' annotatedT == applyOnlyRename s' annotatedT
                then return (rewrite s' ast, str)
                else throwError $ "Expected type is more general than the inferred type"
                    ++ ". Expected type: " ++ AST.prettyPrint (translateFunType p annotatedT)
                    ++ ". Inferred type: " ++ AST.prettyPrint (translateFunType p t') ++ "."
        (AST.FunDeclUntyped identifier args stmts, p) -> tInfFunDecl' p t identifier args stmts
    where
        tInfFunDecl' :: Pos.Pos -> Type -> AST.Identifier -> [AST.Identifier] -> [AST.Statement] -> TInf (AST.FunDecl, String)
        tInfFunDecl' p t identifier args stmts = do
            ctx <- ask
            let newCtx = Stack.stackPush ctx emptyCtx
            scopedCtx <- addArgsToCtx (idName identifier ++ "_") newCtx args -- Create the function's scoped context
            local (const scopedCtx) (do
                t' <- newTypeVar "return"
                (stmts', _) <- tInfStatements t' stmts
                argsTypes <- getArgsTypes args
                t'' <- substitute t'
                let funType = TFunction argsTypes t''
                mgu (Just p) t funType
                funType' <- substitute funType
                let funTypeAST = translateFunType p funType'

                return ((AST.FunDeclTyped identifier args funTypeAST stmts', p), idName identifier))
            where
                addArgsToCtx :: String -> ScopedTypeCtx -> [AST.Identifier] -> TInf ScopedTypeCtx
                addArgsToCtx prefix ctx [] = return ctx
                addArgsToCtx prefix ctx (arg:args) = do
                    typeVar <- newTypeVar (prefix ++ "arg")
                    ctx' <- addArgsToCtx prefix ctx args
                    return $ add ctx' (idName arg) (Scheme [] typeVar)

                getArgsTypes :: [AST.Identifier] -> TInf [Type]
                getArgsTypes [] = return []
                getArgsTypes (arg:args) = do
                    t <- tInfVarName $ idName arg
                    ts <- getArgsTypes args
                    t' <- substitute t
                    return $ t':ts

tInfStatements :: Type -> [AST.Statement] -> TInf ([AST.Statement], Bool)
tInfStatements t [] = do
    mgu Nothing t TVoid
    return ([], False)
tInfStatements t (statement:statements) = do
    t' <- newTypeVar "stmt"
    (statement', varName, returnsValue1) <- tInfStatement t' statement
    t'' <- substitute t'

    -- Update local context if the statement declared a new variable
    ctx <- ask
    let ctx' = (if varName == ""
                    then ctx
                    else add ctx varName (Scheme [] t'')
                )
    local (const ctx') (do
        tRemaining <- newTypeVar "stmts"
        (statements', returnsValue2) <- tInfStatements tRemaining statements

        if returnsValue1
            then if returnsValue2
                then do
                    mgu Nothing t'' tRemaining
                    mgu Nothing t t''
                    s <- substitution
                    return (rewrite s statement : statements', True)
                else do
                    mgu Nothing t t''
                    s <- substitution
                    return (rewrite s statement : statements', True)
            else do
                mgu Nothing t tRemaining
                s <- substitution
                return (rewrite s statement' : statements', False)
        )

tInfManyStatements :: [Type] -> [AST.Statement] -> TInf ()
tInfManyStatements [] [] = void $ mgu Nothing TBool TBool -- todo clean this up
tInfManyStatements (t:ts) (stmt:stmts) = do
    tInfStatement t stmt
    tInfManyStatements ts stmts

tInfStatement :: Type -> AST.Statement -> TInf (AST.Statement, String, Bool)
tInfStatement t (AST.StmtVarDecl decl, p) = do
    (decl', varName) <- tInfVarDecl t decl
    ctx <- ask
    case Stack.stackPeek ctx of
        Just (TypeCtx ctx') ->
            if Map.member varName ctx'
                then throwError $ "Variable multiply defined in scope: " ++ varName ++ show p
                else return ((AST.StmtVarDecl decl', p), varName, False)

tInfStatement t (AST.StmtIf expr st, p) = do
    tInfExpr TBool expr
    (st', varName, returnsValue) <- tInfStatement t st
    return ((AST.StmtIf expr st', p), varName, returnsValue)
tInfStatement t (AST.StmtIfElse expr st1 st2, p) = do
    tInfExpr TBool expr
    t1 <- newTypeVar "st"
    t2 <- newTypeVar "st"
    (st1', varName, returnsValue1) <- tInfStatement t1 st1
    (st2', varName, returnsValue2) <- tInfStatement t2 st2

    if returnsValue1
        then if returnsValue2
            then do
                mgu Nothing t1 t2
                s <- substitution
                return (
                    (AST.StmtIfElse expr (rewrite s st1') (rewrite s st2'), p),
                    "",
                    True)
            else do
                s <- substitution
                return (
                    (AST.StmtIfElse expr (rewrite s st1') st2', p),
                    "",
                    True)
        else do
            s <- substitution
            return (
                (AST.StmtIfElse expr (rewrite s st1') st2', p),
                "",
                False)
tInfStatement t (AST.StmtWhile expr st, p) = do
    tInfExpr TBool expr
    (st', varName, returnsValue) <- tInfStatement t st
    return ((AST.StmtWhile expr st', p), varName, returnsValue)
tInfStatement t (AST.StmtBlock stmts, p) = do
    ctx <- ask
    let newCtx = Stack.stackPush ctx emptyCtx
    local (const newCtx) (do
        (stmts', returnsValue) <- tInfStatements t stmts
        return ((AST.StmtBlock stmts', p), "", returnsValue))
tInfStatement t (AST.StmtAssignment identifier expr, p) = do
    (Scheme _ t) <- getScheme (idName identifier)
    tInfExpr t expr
    return ((AST.StmtAssignment identifier expr, p), "", False)
tInfStatement t (AST.StmtAssignmentField identifier fields expr, p) = do
    (Scheme _ t) <- getScheme (idName identifier)
    t' <- newTypeVar "var"
    tInfExpr t' expr
    tTraverseFields t' t fields
    return ((AST.StmtAssignmentField identifier fields expr, p), "", False)
tInfStatement t (AST.StmtFunCall identifier expressions, p) = do
    tInfExpr t (AST.ExprFunCall identifier expressions, p)
    return ((AST.StmtFunCall identifier expressions, p), "", False)
tInfStatement t (AST.StmtReturn expr, p) = do
    tInfExpr t expr
    return ((AST.StmtReturn expr, p), "", True)
tInfStatement t (AST.StmtReturnVoid, p) = return ((AST.StmtReturnVoid, p), "", True)

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
    dependencies globalDefs (AST.StmtVarDecl decl, _) = dependencies globalDefs decl
    dependencies globalDefs (AST.StmtIf e s, _) =
        let (globalDefs', deps) = dependencies globalDefs e in
            let (_, deps') = dependencies globalDefs' s in
                (globalDefs', deps ++ deps')
    dependencies globalDefs (AST.StmtIfElse e s1 s2, _) =
        let (globalDefs', deps) = dependencies globalDefs e in
            let (_, deps') = dependencies globalDefs' s1 in
                let (_, deps'') = dependencies globalDefs' s2 in
                    (globalDefs', deps ++ deps' ++ deps'')
    dependencies globalDefs (AST.StmtWhile e s, _) =
        let (globalDefs', deps) = dependencies globalDefs e in
            let (_, deps') = dependencies globalDefs' s in
                (globalDefs', deps ++ deps')
    dependencies globalDefs (AST.StmtBlock ss, _) =
        let (_, deps) = dependencies globalDefs ss in
            (globalDefs, deps)
    dependencies globalDefs (AST.StmtAssignment i e, _) =
        let (_, deps) = dependencies globalDefs i in
            let (_, deps') = dependencies globalDefs e in
                (globalDefs, deps ++ deps')
    dependencies globalDefs (AST.StmtAssignmentField i _ e, _) =
        let (_, deps) = dependencies globalDefs i in
            let (_, deps') = dependencies globalDefs e in
                (globalDefs, deps ++ deps')
    dependencies globalDefs (AST.StmtFunCall i es, _) =
        let (_, deps) = dependencies globalDefs i in
            let (_, deps') = dependencies globalDefs es in
                (globalDefs, deps ++ deps')
    dependencies globalDefs (AST.StmtReturn e, _) = dependencies globalDefs e
    dependencies globalDefs (AST.StmtReturnVoid, _) = (globalDefs, [])

instance Dependencies AST.Expression where
    dependencies globalDefs (AST.ExprIdentifier i, _) = dependencies globalDefs i
    dependencies globalDefs (AST.ExprIdentifierField i _, _) = dependencies globalDefs i
    dependencies globalDefs (AST.ExprFunCall i es, _) =
        let (_, deps) = dependencies globalDefs i in
            let (_, deps') = dependencies globalDefs es in
                (globalDefs, deps ++ deps')
    dependencies globalDefs (AST.ExprConstant _, _) = (globalDefs, [])
    dependencies globalDefs (AST.ExprTuple e1 e2, _) =
        let (_, deps) = dependencies globalDefs e1 in
            let (_, deps') = dependencies globalDefs e2 in
                (globalDefs, deps ++ deps')
    dependencies globalDefs (AST.ExprUnaryOp _ e, _) = dependencies globalDefs e
    dependencies globalDefs (AST.ExprBinaryOp _ e1 e2, _) =
        let (_, deps) = dependencies globalDefs e1 in
            let (_, deps') = dependencies globalDefs e2 in
                (globalDefs, deps ++ deps')

instance Dependencies AST.Identifier where
    dependencies globalDefs (AST.Identifier i, _) =
        if i `elem` globalDefs
            then (globalDefs, [i])
            else (globalDefs, [])

------------------------------------------------------------------------------------------------------------------------

translateType :: Pos.Pos -> Type -> AST.Type
translateType p (TVar str)         = (AST.TypeIdentifier (AST.Identifier str, p), p)
translateType p TBool              = (AST.TypeBool, p)
translateType p TInt               = (AST.TypeInt, p)
translateType p TChar              = (AST.TypeChar, p)
translateType p (TList t)          = (AST.TypeList $ translateType p t, p)
translateType p (TTuple t1 t2)     = (AST.TypeTuple (translateType p t1) (translateType p t2), p)
translateType p (TPointer t)       = (AST.TypePointer $ translateType p t, p)

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
rTranslateType (AST.TypePointer t, _)     = TPointer $ rTranslateType t

rTranslateFunType :: AST.FunType -> Type
rTranslateFunType (AST.FunTypeVoid args, _)  = TFunction (map rTranslateType args) TVoid
rTranslateFunType (AST.FunType args body, _) = TFunction (map rTranslateType args) (rTranslateType body)

-- |Class to rewrite the AST with type substitutions
class RewriteAST a where
    rewrite :: Substitution -> a -> a

instance RewriteAST a => RewriteAST [a] where
    rewrite s = map (rewrite s)

instance RewriteAST AST.Type where
    rewrite s (t, p) = translateType p $ apply s $ rTranslateType (t, p)

instance RewriteAST AST.Decl where
    rewrite s (AST.DeclV v, p) = (AST.DeclV (rewrite s v), p)
    rewrite s (AST.DeclF f, p) = (AST.DeclF (rewrite s f), p)

instance RewriteAST AST.VarDecl where
    rewrite s (AST.VarDeclTyped t i e, p) = (AST.VarDeclTyped (rewrite s t) (rewrite s i) (rewrite s e), p)
    rewrite s (AST.VarDeclUntyped i e, p) = (AST.VarDeclUntyped (rewrite s i) (rewrite s e), p)

instance RewriteAST AST.FunDecl where
    rewrite s (AST.FunDeclTyped i is t ss, p) = (AST.FunDeclTyped (rewrite s i) (rewrite s is) (rewrite s t) (rewrite s ss), p)
    rewrite s (AST.FunDeclUntyped i is ss, p) = (AST.FunDeclUntyped (rewrite s i) (rewrite s is) (rewrite s ss), p)

instance RewriteAST AST.FunType where
    rewrite s (AST.FunType ts t, p) = (AST.FunType (rewrite s ts) (rewrite s t), p)
    rewrite s (AST.FunTypeVoid ts, p) = (AST.FunTypeVoid (rewrite s ts), p)

instance RewriteAST AST.Statement where
    rewrite s (AST.StmtVarDecl v, p) = (AST.StmtVarDecl (rewrite s v), p)
    rewrite s (AST.StmtIf e st, p) = (AST.StmtIf (rewrite s e) (rewrite s st), p)
    rewrite s (AST.StmtIfElse e st1 st2, p) = (AST.StmtIfElse (rewrite s e) (rewrite s st1) (rewrite s st2), p)
    rewrite s (AST.StmtWhile e st, p) = (AST.StmtWhile (rewrite s e) (rewrite s st), p)
    rewrite s (AST.StmtBlock sts, p) = (AST.StmtBlock (rewrite s sts), p)
    rewrite s (AST.StmtAssignment i e, p) = (AST.StmtAssignment (rewrite s i) (rewrite s e), p)
    rewrite s (AST.StmtAssignmentField i f e, p) = (AST.StmtAssignmentField (rewrite s i) (rewrite s f) (rewrite s e), p)
    rewrite s (AST.StmtFunCall i es, p) = (AST.StmtFunCall (rewrite s i) (rewrite s es), p)
    rewrite s (AST.StmtReturn e, p) = (AST.StmtReturn (rewrite s e), p)
    rewrite _ st = st

instance RewriteAST AST.Field where
    rewrite _ f = f

instance RewriteAST AST.Expression where
    rewrite _ e = e

instance RewriteAST AST.Identifier where
    rewrite _ i = i
