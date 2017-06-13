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
import Data.Type

------------------------------------------------------------------------------------------------------------------------

check :: Bool -> TypeCtx -> AST.SPL -> Either TInfError AST.SPL
check preserveDeclOrder includedCtx spl = res
    where
        (res, _) = runTInf $ tInfSPL preserveDeclOrder includedCtx spl

checkDet :: Bool -> TypeCtx -> AST.SPL -> AST.SPL
checkDet preserveDeclOrder includedCtx spl =
    case check preserveDeclOrder includedCtx spl of
        Left err -> Trace.trace (show err) undefined
        Right spl' -> spl'

------------------------------------------------------------------------------------------------------------------------

typeInferenceExpr :: (AST.Expression -> TInf Type) -> AST.Expression -> Either TInfError Type
typeInferenceExpr tInf' expr = res
    where
        (res, _) = runTInf $ tInf' expr

------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------

data ValueType = PersistentValue
               | TemporaryValue
                 deriving (Show, Eq)

valueType :: AST.Expression -> ValueType
valueType (AST.ExprIdentifier _, _) = PersistentValue
valueType (AST.ExprField e _, _) = valueType e
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

instance Types a => Types (Maybe a) where
    freeTypeVars (Just m) = freeTypeVars m
    apply _ Nothing = Nothing
    apply s (Just m) = Just $ apply s m
    applyOnlyRename _ Nothing = Nothing
    applyOnlyRename s (Just m) = Just $ applyOnlyRename s m

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
    ("length", let t = TList (TVar "a") in Scheme ["a"] (TFunction [t] TInt)),
    ("malloc", let t = TVar "a" in Scheme ["a"] (TFunction [TInt] (TPointer t)))
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

data TInfError' = TInfErrorUnify Type Type
                | TInfErrorOccursCheck String Type
                | TInfErrorExpectedTypeUnify Type Type
                | TInfErrorExpectedTypeTooGeneral Type Type
                | TInfErrorUnboundVariable String
                | TInfErrorVariableMultiplyDefined String
                | TInfErrorPersistentValueRequired

data TInfError = TInfError TInfError' Pos.Pos

instance Show TInfError where
    show (TInfError err pos) = show err ++ " at " ++ show pos

instance Show TInfError' where
    show (TInfErrorUnify t1 t2) = "Types do not unify: "
        ++ AST.prettyPrint (translateType AST.emptyMeta t1)
        ++ " and "
        ++ AST.prettyPrint (translateType AST.emptyMeta t2)
    show (TInfErrorOccursCheck s t) = "Free type variable "
        ++ s
        ++ "occurs in "
        ++ AST.prettyPrint (translateType AST.emptyMeta t)
    show (TInfErrorExpectedTypeUnify t1 t2) = "Given type and inferred type are not the same. Given: "
        ++ AST.prettyPrint (translateType AST.emptyMeta t1)
        ++ ". Inferred: "
        ++ AST.prettyPrint (translateType AST.emptyMeta t2)
    show (TInfErrorExpectedTypeTooGeneral t1 t2) = "Given type is too general. Given: "
        ++ AST.prettyPrint (translateType AST.emptyMeta t1)
        ++ ". Inferred: "
        ++ AST.prettyPrint (translateType AST.emptyMeta t2)
    show (TInfErrorUnboundVariable s) = "Undefined variable: " ++ s
    show (TInfErrorVariableMultiplyDefined s) = "Variable multiply defined in scope: " ++ s
    show TInfErrorPersistentValueRequired = "Persistent value required for reference (&) operand"

-- |The type inference state consists of the fresh type name generator state and the current type substitution
data TInfState = TInfState { tInfSupply :: Int,
                             tInfSubstitution :: Substitution}
                 deriving (Show)

type TInf a = ExceptT TInfError (ReaderT ScopedTypeCtx (State TInfState)) a

-- |The type inference runner. Results in a tuple of either an error and the type inference result, and the final type
-- inference state. The type inference result will generally be a tuple of a substitution (the type constraints) and the
-- type of an AST branch.
runTInf :: TInf a -> (Either TInfError a, TInfState)
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

-- |Bind a type variable to a type, but don't bind to itself, and make sure the free type variable occurs
varBind :: Pos.Pos -> String -> Type -> TInf Substitution
varBind p u t
    | t == TVar u                   = return nullSubstitution
    | u `Set.member` freeTypeVars t = throwError $ TInfError (TInfErrorOccursCheck u t) p
    | otherwise                     = do
        let s = Map.singleton u t
        st <- get
        put st {tInfSubstitution = s `composeSubstitution` tInfSubstitution st }
        return s

-- |Unify two types (using the most general unifier)
mgu :: AST.Meta -> Type -> Type -> TInf Substitution
mgu m t1 t2 = do
    t1' <- substitute t1
    t2' <- substitute t2
    s <- mgu' (AST.metaPos m) t1' t2'
    t <- substitute t1
    case AST.metaType m of
        Just t' -> do
            --s' <- mgu' (AST.metaPos m) t' t
            s' <- metaMGU m t1'
            return $ s' `composeSubstitution` s
        _ -> return s
    where
    mgu' :: Pos.Pos -> Type -> Type -> TInf Substitution
    mgu' p (TVar u) t                 = varBind p u t
    mgu' p t (TVar u)                 = varBind p u t
    mgu' p TBool TBool                = return nullSubstitution
    mgu' p TInt TInt                  = return nullSubstitution
    mgu' p TChar TChar                = return nullSubstitution
    mgu' p (TList t) (TList t')       = mgu' p t t'
    mgu' p (TTuple t1 t2) (TTuple t1' t2') = do
        s1 <- mgu' p t1 t1'
        s2 <- mgu' p (apply s1 t2) (apply s1 t2')
        return $ s1 `composeSubstitution` s2
    mgu' p (TFunction [] body) (TFunction [] body') = mgu' p body body'
    mgu' p (TFunction (arg:args) body) (TFunction (arg':args') body') = do
        s1 <- mgu' p arg arg'
        s2 <- mgu' p (apply s1 (TFunction args body)) (apply s1 (TFunction args' body'))
        return $ s2 `composeSubstitution` s1
    mgu' p (TPointer t) (TPointer t') = mgu' p t t'
    mgu' p TVoid TVoid                = return nullSubstitution
    mgu' p t1 t2                      = throwError $ TInfError (TInfErrorUnify t1 t2) p

metaMGU :: AST.Meta -> Type -> TInf Substitution
metaMGU m t2 =
    case AST.metaType m of
        Just t1 -> mgu (m {AST.metaType = Nothing}) t1 t2
        _ -> return nullSubstitution

------------------------------------------------------------------------------------------------------------------------

-- |Get the name of an AST identifier
idName :: AST.Identifier -> String
idName (AST.Identifier i, _) = i

getScheme :: Pos.Pos -> String -> TInf Scheme
getScheme p varName = do
    ctx <- ask
    s <- substitution
    let ctx' = apply s ctx
    getScheme' (Stack.stackToList ctx') p varName
    where
        getScheme' :: [TypeCtx] -> Pos.Pos -> String -> TInf Scheme
        getScheme' [] p _ = throwError $ TInfError (TInfErrorUnboundVariable varName) p
        getScheme' (TypeCtx ctx : ctxs) p varName =
            case Map.lookup varName ctx of
                Nothing -> getScheme' ctxs p varName
                Just scheme -> return scheme

tInfVarName :: Pos.Pos -> String -> TInf Type
tInfVarName p varName = do
    scheme <- getScheme p varName
    instantiate scheme
    {-
    ctx <- ask
    s <- substitution
    let ctx' = apply s ctx
    tInfVarName' (Stack.stackToList ctx') p varName
    where
        tInfVarName' :: [TypeCtx] -> Pos.Pos -> String -> TInf Type
        tInfVarName' [] p varName = throwError $ TInfError (TInfErrorUnboundVariable varName) p
        tInfVarName' (TypeCtx ctx : ctxs) p varName =
            case Map.lookup varName ctx of
                Nothing -> tInfVarName' ctxs p varName
                Just scheme -> instantiate scheme
    -}

-- |Perform type inference on an AST identifier
tInfId :: AST.Identifier -> TInf Type
tInfId i@(_, m) = tInfVarName (AST.metaPos m) (idName i)

-- |Perform type inference on an AST constant
tInfConst :: Type -> AST.Constant -> TInf ()
tInfConst t (AST.ConstBool _, m) = void $ mgu m t TBool
tInfConst t (AST.ConstInt _, m) = void $ mgu m t TInt
tInfConst t (AST.ConstChar _, m) = void $ mgu m t TChar
tInfConst t (AST.ConstEmptyList, m) = do
    tVar <- newTypeVar "a"
    void $ mgu m t (TList tVar)


tInfExprTyped :: AST.Expression -> TInf Type
tInfExprTyped e = do
    t <- newTypeVar "t"
    tInfExpr t e
    substitute t

-- |Perform type inference on an AST expression
tInfExpr :: Type -> AST.Expression -> TInf ()
tInfExpr t (AST.ExprIdentifier id, m) = do
    t' <- tInfId id
    void $ mgu m t t'
tInfExpr t (AST.ExprField e fields, m) = do
    metaMGU m t
    t' <- newTypeVar "fld"
    tInfExpr t' e
    tTraverseFields Nothing t t' fields
tInfExpr t (AST.ExprFunCall id@(_, m') args, m) = do
    metaMGU m t
    t1 <- tInfId id
    ts <- mapM (const $ newTypeVar "arg") args
    mgu m' (TFunction ts t) t1
    tInfExprs ts args
tInfExpr t (AST.ExprConstant const, _) = tInfConst t const
tInfExpr t (AST.ExprTuple e1 e2, m) = do
    t1 <- newTypeVar "tuple"
    t2 <- newTypeVar "tuple"
    tInfExpr t1 e1
    tInfExpr t2 e2
    void $ mgu m t (TTuple t1 t2)
tInfExpr t (AST.ExprUnaryOp op e, m) = do
    metaMGU m t
    tInfUnaryOp t op e
tInfExpr t (AST.ExprBinaryOp op e1 e2, m) = do
    metaMGU m t
    tInfBinaryOp t op e1 e2

tTraverseFields :: (Maybe AST.Meta) -> Type -> Type -> [AST.Field] -> TInf ()
tTraverseFields (Just m) t t' [] = void $ mgu (m {AST.metaType = Nothing}) t t'
tTraverseFields _ t t' (field:fields) =
    case field of
        (AST.FieldHd, m) -> do
            tVar <- newTypeVar "fld"
            s <- mgu m t' (TList tVar)
            tTraverseFields (Just m) t (apply s tVar) fields
        (AST.FieldTl, m) -> do
            tVar <- newTypeVar "fld"
            s <- mgu m t' (TList tVar)
            tTraverseFields (Just m) t (apply s (TList tVar)) fields
        (AST.FieldFst, m) -> do
            tVar1 <- newTypeVar "fld"
            tVar2 <- newTypeVar "fld"
            s <- mgu m t' (TTuple tVar1 tVar2)
            tTraverseFields (Just m) t (apply s tVar1) fields
        (AST.FieldSnd, m) -> do
            tVar1 <- newTypeVar "fld"
            tVar2 <- newTypeVar "fld"
            s <- mgu m t' (TTuple tVar1 tVar2)
            tTraverseFields (Just m) t (apply s tVar2) fields

-- |Perform type inference on a list of AST expressions
tInfExprs :: [Type] -> [AST.Expression] -> TInf ()
tInfExprs [] [] = return ()
tInfExprs (t:ts) (expr:exprs) = do
    tInfExpr t expr
    tInfExprs ts exprs

tInfUnaryOp :: Type -> AST.UnaryOperator -> AST.Expression -> TInf ()
tInfUnaryOp t (AST.UnaryOpNeg, m) e = do
    tInfExpr TBool e
    void $ mgu m t TBool
tInfUnaryOp t (AST.UnaryOpBitwiseNot, m) e = do
    tInfExpr TInt e
    void $ mgu m t TInt
tInfUnaryOp t (AST.UnaryOpSubtr, m) e = do
    tInfExpr TInt e
    void $ mgu m t TInt
tInfUnaryOp t (AST.UnaryOpCast castToType, m) e = do
    t' <- newTypeVar "var"
    tInfExpr t' e
    void $ mgu m t (rTranslateType castToType)
tInfUnaryOp t (AST.UnaryOpReference, m) e =
    case valueType e of
        PersistentValue -> do
            t' <- newTypeVar "ref"
            tInfExpr t' e
            void $ mgu m t (TPointer t')
        _ -> throwError $ TInfError TInfErrorPersistentValueRequired (AST.metaPos m)
tInfUnaryOp t (AST.UnaryOpDereference, m) e = tInfExpr (TPointer t) e

tInfBinaryOp :: Type -> AST.BinaryOperator -> AST.Expression -> AST.Expression -> TInf ()
tInfBinaryOp t (AST.BinaryOpOr, m) e1 e2 = do
    tInfExpr TBool e1
    tInfExpr TBool e2
    void $ mgu m t TBool
tInfBinaryOp t (AST.BinaryOpBitwiseOr, m) e1 e2 = do
    tInfExpr TInt e1
    tInfExpr TInt e2
    void $ mgu m t TInt
tInfBinaryOp t (AST.BinaryOpAnd, m) e1 e2 = tInfBinaryOp t (AST.BinaryOpOr, m) e1 e2
tInfBinaryOp t (AST.BinaryOpBitwiseAnd, m) e1 e2 = tInfBinaryOp t (AST.BinaryOpBitwiseOr, m) e1 e2
tInfBinaryOp t (AST.BinaryOpEq, m) e1 e2 = do
    t' <- newTypeVar "expr"
    tInfExpr t' e1
    tInfExpr t' e2
    void $ mgu m t TBool
tInfBinaryOp t (AST.BinaryOpNEq, m) e1 e2 = tInfBinaryOp t (AST.BinaryOpEq, m) e1 e2
tInfBinaryOp t (AST.BinaryOpLT, m) e1 e2 = do
    tInfExpr TInt e1
    tInfExpr TInt e2
    void $ mgu m t TBool
tInfBinaryOp t (AST.BinaryOpGT, m) e1 e2 = tInfBinaryOp t (AST.BinaryOpLT, m) e1 e2
tInfBinaryOp t (AST.BinaryOpLTE, m) e1 e2= tInfBinaryOp t (AST.BinaryOpLT, m) e1 e2
tInfBinaryOp t (AST.BinaryOpGTE, m) e1 e2 = tInfBinaryOp t (AST.BinaryOpLT, m) e1 e2
tInfBinaryOp t (AST.BinaryOpConcat, m) e1 e2 = do
    t' <- newTypeVar "a"
    tInfExpr t' e1
    tInfExpr (TList t') e2
    void $ mgu m t (TList t')
tInfBinaryOp t (AST.BinaryOpPlus, m) e1 e2 = do
    tInfExpr TInt e1
    tInfExpr TInt e2
    void $ mgu m t TInt
tInfBinaryOp t (AST.BinaryOpSubtr, m) e1 e2 = tInfBinaryOp t (AST.BinaryOpPlus, m) e1 e2
tInfBinaryOp t (AST.BinaryOpReferencePlus, m) e1 e2 = do
    t' <- newTypeVar "ref"
    tInfExpr (TPointer t') e1
    tInfExpr TInt e2
    void $ mgu m t (TPointer t')
tInfBinaryOp t (AST.BinaryOpReferenceSubtr, m) e1 e2 = tInfBinaryOp t (AST.BinaryOpReferencePlus, m) e1 e2
tInfBinaryOp t (AST.BinaryOpReferenceReferenceSubtr, m) e1 e2 = do
    t' <- newTypeVar "ref"
    tInfExpr (TPointer t') e1
    tInfExpr (TPointer t') e2
    void $ mgu m t TInt
tInfBinaryOp t (AST.BinaryOpMult, m) e1 e2 = tInfBinaryOp t (AST.BinaryOpPlus, m) e1 e2
tInfBinaryOp t (AST.BinaryOpDiv, m) e1 e2 = tInfBinaryOp t (AST.BinaryOpPlus, m) e1 e2
tInfBinaryOp t (AST.BinaryOpMod, m) e1 e2 = tInfBinaryOp t (AST.BinaryOpPlus, m) e1 e2
tInfBinaryOp t (AST.BinaryOpBitShiftLeft, m) e1 e2 = tInfBinaryOp t (AST.BinaryOpPlus, m) e1 e2
tInfBinaryOp t (AST.BinaryOpBitShiftRight, m) e1 e2 = tInfBinaryOp t (AST.BinaryOpPlus, m) e1 e2

------------------------------------------------------------------------------------------------------------------------

splitDeclsIncludes :: AST.SPL -> (AST.SPL, AST.SPL)
splitDeclsIncludes [] = ([], [])
splitDeclsIncludes (decl:decls) =
    let (decls', includes') = splitDeclsIncludes decls in
        case decl of
            d@(AST.DeclI _, _) -> (decls', d:includes')
            d@(AST.DeclV _, _) -> (d:decls', includes')
            d@(AST.DeclF _, _) -> (d:decls', includes')

-- |Perform type inference on the global SPL declarations. Rewrite the AST such that all variables are typed as
-- completely as possible.
tInfSPL :: Bool -> TypeCtx -> AST.SPL -> TInf AST.SPL
tInfSPL preserveDeclOrder includedCtx decls' = do
    -- Assign fresh type vars to all types in the AST meta
    decls' <- mapMeta metaAssignFreshTypeVar decls'
    -- Split include declarations and regular declarations
    let (decls, includes) = splitDeclsIncludes decls'
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

    let (TypeCtx includedCtx') = includedCtx
    let (TypeCtx builtInCtx') = builtInCtx
    -- Create top scope
    let initCtx = Stack.stackPush ctx (TypeCtx $ Map.union builtInCtx' includedCtx')
    spl <- local (const initCtx) (tInfSCCs decls sccDecls)
    s <- substitution
    st <- get

    mapMeta (\m -> return $ m {AST.metaType = apply s (AST.metaType  m)}) (includes ++ spl)
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
                (AST.DeclV (AST.VarDeclTypedUnitialized _ i, _), _) -> return $ add ctx' (idName i) (Scheme [] typeVar)
                (AST.DeclV (AST.VarDeclUntypedUnitialized i, _), _) -> return $ add ctx' (idName i) (Scheme [] typeVar)
                (AST.DeclF (AST.FunDeclTyped i _ _ _, _), _) -> return $ add ctx' (idName i) (Scheme [] typeVar)
                (AST.DeclF (AST.FunDeclUntyped i _ _, _), _) -> return $ add ctx' (idName i) (Scheme [] typeVar)
        addGlobalToCtx :: ScopedTypeCtx -> AST.Decl -> Type -> TInf ScopedTypeCtx
        addGlobalToCtx ctx decl t =
            case decl of
                (AST.DeclV (AST.VarDeclTyped _ i _, _), _) -> return $ add ctx (idName i) (Scheme [] t)
                (AST.DeclV (AST.VarDeclUntyped i _, _), _) -> return $ add ctx (idName i) (Scheme [] t)
                (AST.DeclV (AST.VarDeclTypedUnitialized _ i, _), _) -> return $ add ctx (idName i) (Scheme [] t)
                (AST.DeclV (AST.VarDeclUntypedUnitialized i, _), _) -> return $ add ctx (idName i) (Scheme [] t)
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
        tInfSCC ((idx, decl@(_,m)):decls) = do
            -- Infer type of the declaration
            t1 <- newTypeVar "decl"
            (decl', varName) <- tInfDecl t1 decl

            -- Get the type scheme of the variable/function we just declared and unify it with the actual type
            (Scheme _ t1') <- getScheme (AST.metaPos m) varName
            mgu m t1' t1

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
tInfDecl t (AST.DeclV decl, m) = do
    (decl', varName) <- tInfVarDecl t decl
    return ((AST.DeclV decl', m), varName)
tInfDecl t (AST.DeclF decl, m) = do
    (decl', varName) <- tInfFunDecl t decl
    return ((AST.DeclF decl', m), varName)

tInfVarDecl :: Type -> AST.VarDecl -> TInf (AST.VarDecl, String)
tInfVarDecl t decl =
    case decl of
        (AST.VarDeclTyped annotatedType identifier expr, m) -> do
            let annotatedT = rTranslateType annotatedType
            (ast, str) <- tInfVarDecl' m t identifier expr
            t' <- substitute t
            s' <- mgu m annotatedT t' `catchError` (\_ ->
                throwError $ TInfError (TInfErrorExpectedTypeUnify annotatedT t') (AST.metaPos m))
            if apply s' annotatedT == applyOnlyRename s' annotatedT
                then return (rewrite s' ast, str)
                else throwError $ TInfError (TInfErrorExpectedTypeTooGeneral annotatedT t') (AST.metaPos m)
        (AST.VarDeclUntyped identifier expr, m) -> tInfVarDecl' m t identifier expr
        (AST.VarDeclTypedUnitialized annotatedType identifier, m) -> do
            let annotatedT = rTranslateType annotatedType
            (ast, str) <- tInfVarDeclUntyped' m t identifier
            t' <- substitute t
            s' <- mgu m annotatedT t' `catchError` (\_ ->
                throwError $ TInfError (TInfErrorExpectedTypeUnify annotatedT t') (AST.metaPos m))
            if apply s' annotatedT == applyOnlyRename s' annotatedT
                then return (rewrite s' ast, str)
                else throwError $ TInfError (TInfErrorExpectedTypeTooGeneral annotatedT t') (AST.metaPos m)
        (AST.VarDeclUntypedUnitialized identifier, m) -> tInfVarDeclUntyped' m t identifier
    where
        tInfVarDecl' :: AST.Meta -> Type -> AST.Identifier -> AST.Expression -> TInf (AST.VarDecl, String)
        tInfVarDecl' m t identifier expr = do
            tInfExpr t expr
            t' <- substitute t
            let t'' = translateType m t'
            return ((AST.VarDeclTyped t'' identifier expr, m), idName identifier)
        tInfVarDeclUntyped' :: AST.Meta-> Type -> AST.Identifier -> TInf (AST.VarDecl, String)
        tInfVarDeclUntyped' m t identifier = do
            let t' = translateType m t
            return ((AST.VarDeclTypedUnitialized t' identifier, m), idName identifier)

tInfFunDecl :: Type -> AST.FunDecl -> TInf (AST.FunDecl, String)
tInfFunDecl t decl =
    case decl of
        (AST.FunDeclTyped identifier args annotatedType stmts, m) -> do
            let annotatedT = rTranslateType annotatedType
            (ast, str) <- tInfFunDecl' m t identifier args stmts
            t' <- substitute t
            s' <- mgu m annotatedT t' `catchError` (\_ ->
                throwError $ TInfError (TInfErrorExpectedTypeUnify annotatedT t') (AST.metaPos m))
            if apply s' annotatedT == applyOnlyRename s' annotatedT
                then return (rewrite s' ast, str)
                else throwError $ TInfError (TInfErrorExpectedTypeTooGeneral annotatedT t') (AST.metaPos m)
        (AST.FunDeclUntyped identifier args stmts, m) -> tInfFunDecl' m t identifier args stmts
    where
        tInfFunDecl' :: AST.Meta -> Type -> AST.Identifier -> [AST.Identifier] -> [AST.Statement] -> TInf (AST.FunDecl, String)
        tInfFunDecl' m t identifier args stmts = do
            ctx <- ask
            let newCtx = Stack.stackPush ctx emptyCtx
            scopedCtx <- addArgsToCtx (idName identifier ++ "_") newCtx args -- Create the function's scoped context
            local (const scopedCtx) (do
                t' <- newTypeVar "return"
                (stmts', _) <- tInfStatements t' stmts
                argsTypes <- getArgsTypes args
                t'' <- substitute t'
                let funType = TFunction argsTypes t''
                mgu m t funType
                funType' <- substitute funType
                let funTypeAST = translateType m funType'

                return ((AST.FunDeclTyped identifier args funTypeAST stmts', m), idName identifier))
            where
                addArgsToCtx :: String -> ScopedTypeCtx -> [AST.Identifier] -> TInf ScopedTypeCtx
                addArgsToCtx prefix ctx [] = return ctx
                addArgsToCtx prefix ctx (arg:args) = do
                    typeVar <- newTypeVar (prefix ++ "arg")
                    ctx' <- addArgsToCtx prefix ctx args
                    return $ add ctx' (idName arg) (Scheme [] typeVar)

                getArgsTypes :: [AST.Identifier] -> TInf [Type]
                getArgsTypes [] = return []
                getArgsTypes (arg@(_, m):args) = do
                    t <- tInfVarName (AST.metaPos m) (idName arg)
                    ts <- getArgsTypes args
                    t' <- substitute t
                    return $ t':ts

tInfStatements :: Type -> [AST.Statement] -> TInf ([AST.Statement], Bool)
tInfStatements t [] = do
    mgu (AST.emptyMeta) t TVoid
    return ([], False)
tInfStatements t (statement@(_,m):statements) = do
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
                    mgu m t'' tRemaining
                    mgu m t t''
                    s <- substitution
                    return (rewrite s statement : statements', True)
                else do
                    mgu m t t''
                    s <- substitution
                    return (rewrite s statement : statements', True)
            else do
                mgu (m {AST.metaType = Nothing}) t tRemaining
                s <- substitution
                return (rewrite s statement' : statements', False)
        )

tInfManyStatements :: [Type] -> [AST.Statement] -> TInf ()
tInfManyStatements [] [] = return ()
tInfManyStatements (t:ts) (stmt:stmts) = do
    tInfStatement t stmt
    tInfManyStatements ts stmts

tInfStatement :: Type -> AST.Statement -> TInf (AST.Statement, String, Bool)
tInfStatement t (AST.StmtVarDecl decl, m) = do
    (decl', varName) <- tInfVarDecl t decl
    ctx <- ask
    case Stack.stackPeek ctx of
        Just (TypeCtx ctx') ->
            if Map.member varName ctx'
                then throwError $ TInfError (TInfErrorVariableMultiplyDefined varName) (AST.metaPos m)
                else return ((AST.StmtVarDecl decl', m), varName, False)

tInfStatement t (AST.StmtIf expr st, m) = do
    tInfExpr TBool expr
    (st', varName, returnsValue) <- tInfStatement t st
    return ((AST.StmtIf expr st', m), varName, returnsValue)
tInfStatement t (AST.StmtIfElse expr st1 st2, m) = do
    tInfExpr TBool expr
    t1 <- newTypeVar "st"
    t2 <- newTypeVar "st"
    (st1', varName, returnsValue1) <- tInfStatement t1 st1
    (st2', varName, returnsValue2) <- tInfStatement t2 st2

    if returnsValue1
        then if returnsValue2
            then do
                mgu m t1 t2
                s <- substitution
                return (
                    (AST.StmtIfElse expr (rewrite s st1') (rewrite s st2'), m),
                    "",
                    True)
            else do
                s <- substitution
                return (
                    (AST.StmtIfElse expr (rewrite s st1') st2', m),
                    "",
                    True)
        else do
            s <- substitution
            return (
                (AST.StmtIfElse expr (rewrite s st1') st2', m),
                "",
                False)
tInfStatement t (AST.StmtWhile expr st, m) = do
    tInfExpr TBool expr
    (st', varName, returnsValue) <- tInfStatement t st
    return ((AST.StmtWhile expr st', m), varName, returnsValue)
tInfStatement t (AST.StmtBlock stmts, m) = do
    ctx <- ask
    let newCtx = Stack.stackPush ctx emptyCtx
    local (const newCtx) (do
        (stmts', returnsValue) <- tInfStatements t stmts
        return ((AST.StmtBlock stmts', m), "", returnsValue))
tInfStatement t (AST.StmtAssignment expr1 expr2, m) = do
    tInfExpr t expr1
    tInfExpr t expr2
    return ((AST.StmtAssignment expr1 expr2, m), "", False)
tInfStatement t (AST.StmtFunCall identifier expressions, m) = do
    tInfExpr t (AST.ExprFunCall identifier expressions, m)
    return ((AST.StmtFunCall identifier expressions, m), "", False)
tInfStatement t (AST.StmtReturn expr, m) = do
    tInfExpr t expr
    return ((AST.StmtReturn expr, m), "", True)
tInfStatement t (AST.StmtReturnVoid, m) = return ((AST.StmtReturnVoid, m), "", True)

------------------------------------------------------------------------------------------------------------------------

class DeclIdentifier a where
    declIdentifier :: a -> String

instance DeclIdentifier AST.Decl where
    declIdentifier (AST.DeclV decl, _) = declIdentifier decl
    declIdentifier (AST.DeclF decl, _) = declIdentifier decl

instance DeclIdentifier AST.VarDecl where
    declIdentifier (AST.VarDeclTyped _ i _, _) = idName i
    declIdentifier (AST.VarDeclUntyped i _, _) = idName i
    declIdentifier (AST.VarDeclTypedUnitialized _ i, _) = idName i
    declIdentifier (AST.VarDeclUntypedUnitialized i, _) = idName i

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
    dependencies globalDefs (AST.VarDeclTypedUnitialized _ i, _) = ([g | g <- globalDefs, g /= idName i], [])
    dependencies globalDefs (AST.VarDeclUntypedUnitialized i, _) = ([g | g <- globalDefs, g /= idName i], [])

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
    dependencies globalDefs (AST.StmtAssignment e1 e2, _) =
        let (_, deps) = dependencies globalDefs e1 in
            let (_, deps') = dependencies globalDefs e2 in
                (globalDefs, deps ++ deps')
    dependencies globalDefs (AST.StmtFunCall i es, _) =
        let (_, deps) = dependencies globalDefs i in
            let (_, deps') = dependencies globalDefs es in
                (globalDefs, deps ++ deps')
    dependencies globalDefs (AST.StmtReturn e, _) = dependencies globalDefs e
    dependencies globalDefs (AST.StmtReturnVoid, _) = (globalDefs, [])

instance Dependencies AST.Expression where
    dependencies globalDefs (AST.ExprIdentifier i, _) = dependencies globalDefs i
    dependencies globalDefs (AST.ExprField e _, _) = dependencies globalDefs e
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

translateType :: AST.Meta -> Type -> AST.Type
translateType m (TVar str)         = (AST.TypeIdentifier (AST.Identifier str, m), m)
translateType m TVoid              = (AST.TypeVoid, m)
translateType m TBool              = (AST.TypeBool, m)
translateType m TInt               = (AST.TypeInt, m)
translateType m TChar              = (AST.TypeChar, m)
translateType m (TList t)          = (AST.TypeList $ translateType m t, m)
translateType m (TTuple t1 t2)     = (AST.TypeTuple (translateType m t1) (translateType m t2), m)
translateType m (TPointer t)       = (AST.TypePointer $ translateType m t, m)
translateType m (TFunction args t) = (AST.TypeFunction (map (translateType m) args) (translateType m t), m)

rTranslateType :: AST.Type -> Type
rTranslateType (AST.TypeIdentifier id, _)   = TVar $ idName id
rTranslateType (AST.TypeVoid, _)            = TVoid
rTranslateType (AST.TypeBool, _)            = TBool
rTranslateType (AST.TypeInt, _)             = TInt
rTranslateType (AST.TypeChar, _)            = TChar
rTranslateType (AST.TypeList t, _)          = TList $ rTranslateType t
rTranslateType (AST.TypeTuple t1 t2, _)     = TTuple (rTranslateType t1) (rTranslateType t2)
rTranslateType (AST.TypePointer t, _)       = TPointer $ rTranslateType t
rTranslateType (AST.TypeFunction args t, _) = TFunction (map rTranslateType args) (rTranslateType t)

-- |Assign fresh type var to meta
metaAssignFreshTypeVar :: AST.Meta -> TInf AST.Meta
metaAssignFreshTypeVar m = do
    t <- newTypeVar "meta"
    return m {AST.metaType = Just t}

-- |Class to rewrite the AST with type information
class RewriteAST a where
    rewrite :: Substitution -> a -> a
    mapMeta :: (AST.Meta -> TInf AST.Meta) -> a -> TInf a

instance RewriteAST a => RewriteAST [a] where
    rewrite s = map (rewrite s)
    mapMeta f = mapM (mapMeta f)

instance RewriteAST AST.Type where
    rewrite s (t, m) = translateType m $ apply s $ rTranslateType (t, m)

    mapMeta f (AST.TypeTuple t1 t2, m) = do
        m' <- f m
        t1' <- mapMeta f t1
        t2' <- mapMeta f t2
        return (AST.TypeTuple t1' t2', m')
    mapMeta f (AST.TypeList t, m) = do
        m' <- f m
        t' <- mapMeta f t
        return (AST.TypeList t', m')
    mapMeta f (AST.TypeIdentifier i, m) = do
        m' <- f m
        i' <- mapMeta f i
        return (AST.TypeIdentifier i', m')
    mapMeta f (AST.TypePointer t, m) = do
        m' <- f m
        t' <- mapMeta f t
        return (AST.TypePointer t', m')
    mapMeta f (AST.TypeFunction ts t, m) = do
        m' <- f m
        ts' <- mapMeta f ts
        t' <- mapMeta f t
        return (AST.TypeFunction ts' t', m')
    mapMeta f (a, m) = f m >>= \m' -> return (a, m')

instance RewriteAST AST.Decl where
    rewrite s (AST.DeclV v, m) = (AST.DeclV (rewrite s v), m)
    rewrite s (AST.DeclF f, m) = (AST.DeclF (rewrite s f), m)

    mapMeta f (AST.DeclV v, m) = do
        m' <- f m
        v' <- mapMeta f v
        return (AST.DeclV v', m')
    mapMeta f (AST.DeclF fun, m) = do
        m' <- f m
        fun' <- mapMeta f fun
        return (AST.DeclF fun', m')

instance RewriteAST AST.VarDecl where
    rewrite s (AST.VarDeclTyped t i e, m) = (AST.VarDeclTyped (rewrite s t) (rewrite s i) (rewrite s e), m)
    rewrite s (AST.VarDeclUntyped i e, m) = (AST.VarDeclUntyped (rewrite s i) (rewrite s e), m)
    rewrite s (AST.VarDeclTypedUnitialized t i, m) = (AST.VarDeclTypedUnitialized (rewrite s t) (rewrite s i), m)
    rewrite s (AST.VarDeclUntypedUnitialized i, m) = (AST.VarDeclUntypedUnitialized (rewrite s i), m)

    mapMeta f (AST.VarDeclTyped t i e, m) = do
        m' <- f m
        t' <- mapMeta f t
        i' <- mapMeta f i
        e' <- mapMeta f e
        return (AST.VarDeclTyped t' i' e', m')
    mapMeta f (AST.VarDeclUntyped i e, m) = do
        m' <- f m
        i' <- mapMeta f i
        e' <- mapMeta f e
        return (AST.VarDeclUntyped i' e', m')
    mapMeta f (AST.VarDeclTypedUnitialized t i, m) = do
        m' <- f m
        t' <- mapMeta f t
        i' <- mapMeta f i
        return (AST.VarDeclTypedUnitialized t i', m')
    mapMeta f (AST.VarDeclUntypedUnitialized i, m) = do
        m' <- f m
        i' <- mapMeta f i
        return (AST.VarDeclUntypedUnitialized i', m')

instance RewriteAST AST.FunDecl where
    rewrite s (AST.FunDeclTyped i is t ss, m) = (AST.FunDeclTyped (rewrite s i) (rewrite s is) (rewrite s t) (rewrite s ss), m)
    rewrite s (AST.FunDeclUntyped i is ss, m) = (AST.FunDeclUntyped (rewrite s i) (rewrite s is) (rewrite s ss), m)

    mapMeta f (AST.FunDeclTyped i is t ss, m) = do
        m' <- f m
        i' <- mapMeta f i
        is' <- mapMeta f is
        t' <- mapMeta f t
        ss' <- mapMeta f ss
        return (AST.FunDeclTyped i' is' t' ss', m')
    mapMeta f (AST.FunDeclUntyped i is ss, m) = do
        m' <- f m
        i' <- mapMeta f i
        is' <- mapMeta f is
        ss' <- mapMeta f ss
        return (AST.FunDeclUntyped i' is' ss', m')

instance RewriteAST AST.Statement where
    rewrite s (AST.StmtVarDecl v, m) = (AST.StmtVarDecl (rewrite s v), m)
    rewrite s (AST.StmtIf e st, m) = (AST.StmtIf (rewrite s e) (rewrite s st), m)
    rewrite s (AST.StmtIfElse e st1 st2, m) = (AST.StmtIfElse (rewrite s e) (rewrite s st1) (rewrite s st2), m)
    rewrite s (AST.StmtWhile e st, m) = (AST.StmtWhile (rewrite s e) (rewrite s st), m)
    rewrite s (AST.StmtBlock sts, m) = (AST.StmtBlock (rewrite s sts), m)
    rewrite s (AST.StmtAssignment e1 e2, m) = (AST.StmtAssignment (rewrite s e1) (rewrite s e2), m)
    rewrite s (AST.StmtFunCall i es, m) = (AST.StmtFunCall (rewrite s i) (rewrite s es), m)
    rewrite s (AST.StmtReturn e, m) = (AST.StmtReturn (rewrite s e), m)
    rewrite _ st = st

    mapMeta f (AST.StmtVarDecl v, m) = do
        m' <- f m
        v' <- mapMeta f v
        return (AST.StmtVarDecl v', m')
    mapMeta f (AST.StmtIf e st, m) = do
        m' <- f m
        e' <- mapMeta f e
        st' <- mapMeta f st
        return (AST.StmtIf e' st', m')
    mapMeta f (AST.StmtIfElse e st1 st2, m) = do
        m' <- f m
        e' <- mapMeta f e
        st1' <- mapMeta f st1
        st2' <- mapMeta f st2
        return (AST.StmtIfElse e' st1' st2', m')
    mapMeta f (AST.StmtWhile e st, m) = do
        m' <- f m
        e' <- mapMeta f e
        st' <- mapMeta f st
        return (AST.StmtWhile e' st', m')
    mapMeta f (AST.StmtBlock sts, m) = do
        m' <- f m
        sts' <- mapMeta f sts
        return (AST.StmtBlock sts', m')
    mapMeta f (AST.StmtAssignment e1 e2, m) = do
        m' <- f m
        e1' <- mapMeta f e1
        e2' <- mapMeta f e2
        return (AST.StmtAssignment e1' e2', m')
    mapMeta f (AST.StmtFunCall i es, m) = do
        m' <- f m
        i' <- mapMeta f i
        es' <- mapMeta f es
        return (AST.StmtFunCall i' es', m')
    mapMeta f (AST.StmtReturn e, m) = do
        m' <- f m
        e' <- mapMeta f e
        return (AST.StmtReturn e', m')

instance RewriteAST AST.Field where
    rewrite _ f = f

    mapMeta f (a, m) = f m >>= \m' -> return (a, m')

instance RewriteAST AST.Expression where
    rewrite _ e = e

    mapMeta f (AST.ExprIdentifier i, m) = do
        m' <- f m
        i' <- mapMeta f i
        return (AST.ExprIdentifier i', m')
    mapMeta f (AST.ExprField e fs, m) = do
        m' <- f m
        e' <- mapMeta f e
        fs' <- mapMeta f fs
        return (AST.ExprField e' fs', m')
    mapMeta f (AST.ExprFunCall i es, m) = do
        m' <- f m
        i' <- mapMeta f i
        es' <- mapMeta f es
        return (AST.ExprFunCall i' es', m')
    mapMeta f (AST.ExprConstant c, m) = do
        m' <- f m
        c' <- mapMeta f c
        return (AST.ExprConstant c', m')
    mapMeta f (AST.ExprTuple e1 e2, m) = do
        m' <- f m
        e1' <- mapMeta f e1
        e2' <- mapMeta f e2
        return (AST.ExprTuple e1' e2', m')
    mapMeta f (AST.ExprUnaryOp op e, m) = do
        m' <- f m
        op' <- mapMeta f op
        e' <- mapMeta f e
        return (AST.ExprUnaryOp op' e', m')
    mapMeta f (AST.ExprBinaryOp op e1 e2, m) = do
        m' <- f m
        op' <- mapMeta f op
        e1' <- mapMeta f e1
        e2' <- mapMeta f e2
        return (AST.ExprBinaryOp op' e1' e2', m')

instance RewriteAST AST.Constant where
    rewrite = undefined

    mapMeta f (AST.ConstBool b, m) = f m >>= \m' -> return (AST.ConstBool b, m')
    mapMeta f (AST.ConstInt n, m) = f m >>= \m' -> return (AST.ConstInt n, m')
    mapMeta f (AST.ConstChar c, m) = f m >>= \m' -> return (AST.ConstChar c, m')
    mapMeta f (AST.ConstEmptyList, m) = f m >>= \m' -> return (AST.ConstEmptyList, m')

instance RewriteAST AST.UnaryOperator where
    rewrite = undefined

    mapMeta f (AST.UnaryOpCast t, m) = f m >>= \m' -> return (AST.UnaryOpCast t, m')
    mapMeta f (op, m) = f m >>= \m' -> return (op, m')

instance RewriteAST AST.BinaryOperator where
    rewrite = undefined

    mapMeta f (op, m) = f m >>= \m' -> return (op, m')

instance RewriteAST AST.Identifier where
    rewrite _ i = i

    mapMeta f (i, m) = f m >>= \m' -> return (i, m')
