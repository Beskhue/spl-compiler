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

import qualified Data.Char as Char

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

typeInferenceExpr :: (Type -> AST.Expression -> TInf Type) -> AST.Expression -> Either TInfError Type
typeInferenceExpr tInf' expr = res
    where
        (res, _) = runTInf $ (tInf' $ TVar "a") expr

------------------------------------------------------------------------------------------------------------------------

data ValueType = PersistentValue
               | TemporaryValue
                 deriving (Show, Eq)

valueType :: AST.Expression -> ValueType
valueType (AST.ExprIdentifier _, _) = PersistentValue
valueType (AST.ExprField e _, _) = valueType e
valueType (AST.ExprUnaryOp (AST.UnaryOpDereference, _) e, _) = valueType e
valueType (AST.ExprUnaryOp (AST.UnaryOpCast _, _) e, _) = valueType e
valueType (AST.ExprClassMember e _, _) = valueType e
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
    freeTypeVars TType = Set.empty
    freeTypeVars (TClass t) = freeTypeVars t
    freeTypeVars (TClassIdentifier _) = Set.empty

    apply s (TVar v) =
        case Map.lookup v s of
            Just t -> t
            _ -> TVar v
    apply _ TBool                  = TBool
    apply _ TInt                   = TInt
    apply _ TChar                  = TChar
    apply s (TList l)              = TList $ apply s l
    apply s (TTuple t1 t2)         = TTuple (apply s t1) (apply s t2)
    apply s (TFunction arg body)   = TFunction (apply s arg) (apply s body)
    apply s (TPointer t)           = TPointer $ apply s t
    apply _ TVoid                  = TVoid
    apply _ TType                  = TType
    apply s (TClass t)             = TClass $ apply s t
    apply _ t@(TClassIdentifier _) = t

    applyOnlyRename s (TVar v) =
        case Map.lookup v s of
            Just (TVar v') -> TVar v'
            _ -> TVar v
    applyOnlyRename _ TBool                  = TBool
    applyOnlyRename _ TInt                   = TInt
    applyOnlyRename _ TChar                  = TChar
    applyOnlyRename s (TList l)              = TList $ applyOnlyRename s l
    applyOnlyRename s (TTuple t1 t2)         = TTuple (applyOnlyRename s t1) (applyOnlyRename s t2)
    applyOnlyRename s (TFunction arg body)   = TFunction (applyOnlyRename s arg) (applyOnlyRename s body)
    applyOnlyRename s (TPointer t)           = TPointer $ applyOnlyRename s t
    applyOnlyRename _ TVoid                  = TVoid
    applyOnlyRename _ TType                  = TType
    applyOnlyRename s (TClass t)             = TClass $ applyOnlyRename s t
    applyOnlyRename _ t@(TClassIdentifier _) = t

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
-- apply (s1 `composeSubstitution` s2) == apply s1 . apply s2
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
    ("println", let t = TVar "a" in Scheme ["a"] (TFunction [t] TBool)),
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
                | TInfErrorCannotInferClass
                | TInfErrorGeneric String
                  deriving (Eq)

data TInfError = TInfError TInfError' Pos.Pos
                 deriving (Eq)

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
    show TInfErrorPersistentValueRequired = "Persistent value required"
    show TInfErrorCannotInferClass = "Can not infer class of expression"
    show (TInfErrorGeneric s) = "An error occurred: " ++ s

-- Create a map from identifiers to the class they belong to and their original identifiers
type DeclClassMap = Map.Map AST.Identifier (AST.ClassIdentifier, AST.Identifier)

-- |The type inference state consists of the fresh type name generator state and the current type substitution
data TInfState = TInfState { tInfSupply :: Int,
                             tInfSubstitution :: Substitution,
                             tInfDeclClassMap :: DeclClassMap }
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
                                    tInfSubstitution = Map.empty,
                                    tInfDeclClassMap = Map.empty}

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

declClassMap :: TInf DeclClassMap
declClassMap = do
    s <- get
    return $ tInfDeclClassMap s

setDeclClass :: AST.Identifier -> (AST.ClassIdentifier, AST.Identifier) -> TInf ()
setDeclClass i clss = do
    m <- declClassMap
    let m' = Map.insert i clss m
    s <- get
    put s {tInfDeclClassMap = m'}

getDeclClass :: AST.Identifier -> TInf (Maybe (AST.ClassIdentifier, AST.Identifier))
getDeclClass i = do
    m <- declClassMap
    return $ Map.lookup i m

getClassDecls :: AST.ClassIdentifier -> TInf [AST.Identifier]
getClassDecls classIdentifier = do
    m <- declClassMap
    return [i | (i, (classIdentifier', _)) <- Map.toList m, classIdentifier == classIdentifier']
    {-
    return $ concatMap (\(i, (classIdentifier', _)) ->
            if classIdentifier == classIdentifier'
                then [i]
                else []) (Map.toList m)
    -}

------------------------------------------------------------------------------------------------------------------------

-- |Bind a type variable to a type, but don't bind to itself, and make sure the free type variable occurs
varBind :: Pos.Pos -> String -> Type -> TInf Substitution
varBind p u t
    | t == TVar u                   = return nullSubstitution
    | u `Set.member` freeTypeVars t = throwError $ TInfError (TInfErrorOccursCheck u t) p
    | otherwise                     = return $ Map.singleton u t

-- |Unify two types (using the most general unifier)
mgu :: AST.Meta -> Type -> Type -> TInf Substitution
mgu m t1 t2 = do
    t1' <- substitute t1
    t2' <- substitute t2
    s1 <- mgu' (AST.metaPos m) t1' t2'
    t <- substitute t1'
    s2 <- metaMGU m t

    let sCombined = s2 `composeSubstitution` s1
    st <- get
    let sNew = sCombined `composeSubstitution` tInfSubstitution st
    put st {tInfSubstitution = sNew}
    return sCombined
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
        return $ s2 `composeSubstitution` s1
    mgu' p (TFunction [] body) (TFunction [] body') = mgu' p body body'
    mgu' p (TFunction (arg:args) body) (TFunction (arg':args') body') = do
        s1 <- mgu' p arg arg'
        s2 <- mgu' p (apply s1 (TFunction args body)) (apply s1 (TFunction args' body'))
        return $ s2 `composeSubstitution` s1
    mgu' p (TPointer t) (TPointer t') = mgu' p t t'
    mgu' p TVoid TVoid                = return nullSubstitution
    mgu' p TType TType                = return nullSubstitution
    mgu' p (TClass t) (TClass t')     = mgu' p t t'
    mgu' p t@(TClassIdentifier i) t'@(TClassIdentifier i') = if i == i'
        then return nullSubstitution
        else throwError $ TInfError (TInfErrorUnify t t') p
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

classIDName :: AST.ClassIdentifier -> String
classIDName (AST.ClassIdentifier i, _) = i

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

-- |Perform type inference on an AST identifier
tInfId :: Type -> AST.Identifier -> TInf ()
tInfId t i@(_, m) = do
    t' <- tInfVarName (AST.metaPos m) (idName i)
    void $ mgu m t t'

tInfClassId :: Type -> AST.ClassIdentifier -> TInf ()
tInfClassId t i@(_, m) = do
    t' <- tInfVarName (AST.metaPos m) (classIDName i)
    void $ mgu m t t'

-- |Perform type inference on an AST constant
tInfConst :: Type -> AST.Constant -> TInf ()
tInfConst t (AST.ConstBool _, m) = void $ mgu m t TBool
tInfConst t (AST.ConstInt _, m) = void $ mgu m t TInt
tInfConst t (AST.ConstChar _, m) = void $ mgu m t TChar
tInfConst t (AST.ConstEmptyList, m) = do
    tVar <- newTypeVar "a"
    void $ mgu m t (TList tVar)


tInfExprTyped :: Type -> AST.Expression -> TInf Type
tInfExprTyped t e = do
    tInfExpr t e
    substitute t

-- |Perform type inference on an AST expression
tInfExpr :: Type -> AST.Expression -> TInf ()
tInfExpr t (AST.ExprIdentifier i, m) = do
    metaMGU m t
    tInfId t i
tInfExpr t (AST.ExprField e fields, m) = do
    metaMGU m t
    t' <- newTypeVar "fld"
    tInfExpr t' e
    tTraverseFields Nothing t t' fields
tInfExpr t (AST.ExprFunCall e@(_, m') args, m) = do
    metaMGU m t
    case valueType e of
        PersistentValue -> do
            ts <- mapM (const $ newTypeVar "arg") args
            tInfExpr (TFunction ts t) e
            tInfExprs ts args
        _ -> throwError $ TInfError TInfErrorPersistentValueRequired (AST.metaPos m)
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
tInfExpr t (AST.ExprClassConstructor i args, m) = do
    let m' = m {AST.metaType = Nothing}
    let newIdentifier = classIDName i ++ "." ++ "__init__"

    -- Type inference/checking on the __init__ method of the class
    t' <- tInfVarName (AST.metaPos m) newIdentifier
    ts <- mapM (const $ newTypeVar "arg") args
    mgu m' (TFunction (TPointer (TClass $ TClassIdentifier $ classIDName i) : ts) TVoid) t'

    -- Type inference/checking on the constructor arguments
    t' <- substitute t'
    let TFunction argTypes _ = t'
    tInfExprs (tail argTypes) args

    -- Type of the constructor expression
    void $ mgu m t (TClass $ TClassIdentifier $ classIDName i)

tInfExpr t (AST.ExprNew e, m) = do
    t' <- newTypeVar "a"
    tInfExpr t' e
    void $ mgu m t (TPointer t')
tInfExpr t (AST.ExprDelete e, m) = do
    t' <- newTypeVar "a"
    tInfExpr (TPointer t') e
    void $ mgu m t TVoid
tInfExpr t (AST.ExprClassMember e@(_, m') i, m) = do
    metaMGU m t
    t' <- newTypeVar "class"
    tInfExpr t' e
    t'' <- substitute t'
    case t'' of
        TClass (TClassIdentifier clss) -> do
            let newIdentifier = (AST.Identifier $ clss ++ "." ++ idName i, AST.emptyMeta)
            tInfId t newIdentifier
        _ -> throwError $ TInfError (TInfErrorGeneric "unable to access member; could not infer class. Consider making the expected object class explicit.") (AST.metaPos m')

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
tInfUnaryOp t (AST.UnaryOpDereference, m) e = do
    metaMGU m t
    tInfExpr (TPointer t) e

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
            d@(AST.DeclC _, _) -> (d:decls', includes')
            d@(AST.DeclV _, _) -> (d:decls', includes')
            d@(AST.DeclF _, _) -> (d:decls', includes')

addClassDeclsToGlobals :: AST.SPL -> TInf AST.SPL
addClassDeclsToGlobals [] = return []
addClassDeclsToGlobals (d@(AST.DeclC (AST.ClassDecl i vs fs, _), _):ds) = do
    vs' <- renameVarDecls i vs
    fs' <- renameFunDecls i fs
    ds' <- addClassDeclsToGlobals ds
    return $ d : (vs' ++ fs' ++ ds')
    where
        renameVarDecls :: AST.ClassIdentifier -> [AST.VarDecl] -> TInf [AST.Decl]
        renameVarDecls _ [] = return []
        renameVarDecls i (d@(decl, m):ds) = do
            ds' <- renameVarDecls i ds
            d' <- case decl of
                    AST.VarDeclUntyped i'@(_, m') e' -> do
                        let newIdentifier = (AST.Identifier $ classIDName i ++ "." ++ idName i', m')
                        setDeclClass newIdentifier (i, i')
                        return (AST.VarDeclUntyped newIdentifier e', m)
                    AST.VarDeclTyped t' i'@(_, m') e' -> do
                        let newIdentifier = (AST.Identifier $ classIDName i ++ "." ++ idName i', m')
                        setDeclClass newIdentifier (i, i')
                        return (AST.VarDeclTyped t' newIdentifier e', m)
                    AST.VarDeclUntypedUnitialized i'@(_, m') -> do
                        let newIdentifier = (AST.Identifier $ classIDName i ++ "." ++ idName i', m')
                        setDeclClass newIdentifier (i, i')
                        return (AST.VarDeclUntypedUnitialized newIdentifier, m)
                    AST.VarDeclTypedUnitialized t' i'@(_, m') -> do
                        let newIdentifier = (AST.Identifier $ classIDName i ++ "." ++ idName i', m')
                        setDeclClass newIdentifier (i, i')
                        return (AST.VarDeclTypedUnitialized t' newIdentifier, m)
            return $ (AST.DeclV d', m) : ds'
        renameFunDecls :: AST.ClassIdentifier -> [AST.FunDecl] -> TInf [AST.Decl]
        renameFunDecls _ [] = return []
        renameFunDecls i (d@(decl, m):ds) = do
            ds' <- renameFunDecls i ds
            d' <- case decl of
                AST.FunDeclUntyped i'@(_, m') is' ss' -> do
                    let newIdentifier = (AST.Identifier $ classIDName i ++ "." ++ idName i', m')
                    setDeclClass newIdentifier (i, i')
                    return (AST.FunDeclUntyped newIdentifier is' ss', m)
                AST.FunDeclTyped i'@(_, m') is' t' ss' -> do
                    let newIdentifier = (AST.Identifier $ classIDName i ++ "." ++ idName i', m')
                    setDeclClass newIdentifier (i, i')
                    return (AST.FunDeclTyped newIdentifier is' t' ss', m)
            return $ (AST.DeclF d', m) : ds'
addClassDeclsToGlobals (d@(_, m):ds) = do
    ds' <- addClassDeclsToGlobals ds
    return $ d : ds'

-- |Perform type inference on the global SPL declarations. Rewrite the AST such that all variables are typed as
-- completely as possible.
tInfSPL :: Bool -> TypeCtx -> AST.SPL -> TInf AST.SPL
tInfSPL preserveDeclOrder includedCtx decls' = do
    -- Assign fresh type vars to all types in the AST meta
    decls' <- mapMeta metaAssignFreshTypeVar decls'
    -- Split include declarations and regular declarations
    let (decls'', includes) = splitDeclsIncludes decls'
    -- Add class declarations to the globals
    decls <- addClassDeclsToGlobals decls''
    -- Determine the variable and method dependencies
    let deps = tInfSPLGraph decls
    -- Expand class dependencies
    dClassMap <- liftM Map.toList declClassMap
    let classes = map (classIDName . fst . snd) dClassMap
    let deps' = map (\(d, s, ds) ->
            (d, s, concatMap (\dep ->
                    if dep `elem` classes
                        then dep : [idName dep' | (dep', (classIdentifier, _)) <- dClassMap, classIDName classIdentifier == dep]
                        else [dep]) ds)) deps
    let (graph, vertexToEdge, keyToVertex) = Graph.graphFromEdges deps'
    -- Determine the strongly connected components
    let (sccTopo, _) = Graph.SCC.scc graph
    let scc = reverse sccTopo -- Strongly connected components in reverse topological order ([(sccId, [keys])])
    -- Calculate list of strongly connected declarations and, if declration order is to be preserved, the original location of the declarations
    -- [[(originalIndex, decl)]]
    let sccDecls = if preserveDeclOrder
        then map (map (\vertex -> (\d@(decl, _, _) -> (case elemIndex d deps' of Just idx -> idx, decl)) $ vertexToEdge vertex) . snd) scc
        else numberAscending $ map (map (\vertex -> let d@(decl, _, _) = vertexToEdge vertex in decl) . snd) scc

    --let (_, meta) = head decls'
    --let p = AST.metaPos meta
    --when (Pos.sourceName p == "example-programs/test_smart.spl") (
    --    throwError $ TInfError (TInfErrorGeneric $ show dClassMap) Pos.emptyPos)
    --    throwError $ TInfError (TInfErrorGeneric $ show includedCtx) Pos.emptyPos)
    --    throwError $ TInfError (TInfErrorGeneric $ show sccDecls) (Pos.emptyPos))

    let (TypeCtx includedCtx') = includedCtx
    let (TypeCtx builtInCtx') = builtInCtx
    -- Create top scope
    ctx <- ask
    let initCtx = Stack.stackPush ctx (TypeCtx $ Map.union builtInCtx' includedCtx')
    spl <- local (const initCtx) (tInfSCCs decls sccDecls)
    s <- substitution
    st <- get
    -- Rewrite class member declarations (currently in global scope) back into
    -- the class declarations.
    spl' <- rewriteClassDecls spl
    spl'' <- removeGlobalScopeClassDecls spl'
    -- Rewrite types in the meta
    mapMeta (\m -> return $ m {AST.metaType = apply s (AST.metaType m)}) (includes ++ spl'')
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
                (AST.DeclC (AST.ClassDecl i _ _, _), _) -> return $ add ctx' (classIDName i) (Scheme [] typeVar)
                (AST.DeclV (AST.VarDeclTyped _ i _, _), _) -> return $ add ctx' (idName i) (Scheme [] typeVar)
                (AST.DeclV (AST.VarDeclUntyped i _, _), _) -> return $ add ctx' (idName i) (Scheme [] typeVar)
                (AST.DeclV (AST.VarDeclTypedUnitialized _ i, _), _) -> return $ add ctx' (idName i) (Scheme [] typeVar)
                (AST.DeclV (AST.VarDeclUntypedUnitialized i, _), _) -> return $ add ctx' (idName i) (Scheme [] typeVar)
                (AST.DeclF (AST.FunDeclTyped i _ _ _, _), _) -> return $ add ctx' (idName i) (Scheme [] typeVar)
                (AST.DeclF (AST.FunDeclUntyped i _ _, _), _) -> return $ add ctx' (idName i) (Scheme [] typeVar)
        addGlobalToCtx :: ScopedTypeCtx -> AST.Decl -> Type -> TInf ScopedTypeCtx
        addGlobalToCtx ctx decl t =
            case decl of
                (AST.DeclC (AST.ClassDecl i _ _, _), _) -> return $ add ctx (classIDName i) (Scheme [] t)
                (AST.DeclV (AST.VarDeclTyped _ i _, _), _) -> if (Char.isUpper . head . idName) i
                    then return $ add ctx (idName i) (generalize ctx t)
                    else return $ add ctx (idName i) (Scheme [] t)
                (AST.DeclV (AST.VarDeclUntyped i _, _), _) -> if (Char.isUpper . head . idName) i
                    then return $ add ctx (idName i) (generalize ctx t)
                    else return $ add ctx (idName i) (Scheme [] t)
                (AST.DeclV (AST.VarDeclTypedUnitialized _ i, _), _) -> if (Char.isUpper . head . idName) i
                    then return $ add ctx (idName i) (generalize ctx t)
                    else return $ add ctx (idName i) (Scheme [] t)
                (AST.DeclV (AST.VarDeclUntypedUnitialized i, _), _) -> if (Char.isUpper . head . idName) i
                    then return $ add ctx (idName i) (generalize ctx t)
                    else return $ add ctx (idName i) (Scheme [] t)
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
        rewriteClassDecls :: AST.SPL -> TInf AST.SPL
        rewriteClassDecls spl = do
            let assocList = map (\d ->
                    (case declIdentifier d of
                        Left i -> i
                        _ -> undefined, d)) (filter (\d -> case declIdentifier d of
                            Left _ -> True
                            Right _ -> False) spl)
            rewriteClassDecls' (Map.fromList assocList) spl
            where
            rewriteClassDecls' :: Map.Map AST.Identifier AST.Decl -> AST.SPL -> TInf AST.SPL
            rewriteClassDecls' _ [] = return []
            rewriteClassDecls' declMap (d@(AST.DeclC (AST.ClassDecl i vs fs, m'), m):ds) = do
                vs' <- rewriteClassVarDecls' declMap i vs
                fs' <- rewriteClassFunDecls' declMap i fs
                ds' <- rewriteClassDecls' declMap ds
                return ((AST.DeclC (AST.ClassDecl i vs' fs', m'), m) : ds')
                where
                    rewriteClassVarDecls' :: Map.Map AST.Identifier AST.Decl -> AST.ClassIdentifier -> [AST.VarDecl] -> TInf [AST.VarDecl]
                    rewriteClassVarDecls' _ _ [] = return []
                    rewriteClassVarDecls' declMap i (d : ds) = do
                        ds' <- rewriteClassVarDecls' declMap i ds
                        let (Left i'@(_, m'')) = declIdentifier d
                        let newIdentifier = (AST.Identifier $ classIDName i ++ "." ++ idName i', m'')
                        let (Just (AST.DeclV d', _)) = Map.lookup newIdentifier declMap
                        let d'' = case d' of
                                (AST.VarDeclTyped t _ e, m''') -> (AST.VarDeclTyped t i' e, m''')
                                (AST.VarDeclTypedUnitialized t _, m''') -> (AST.VarDeclTypedUnitialized t i', m''')
                        return $ d'' : ds'
                    rewriteClassFunDecls' :: Map.Map AST.Identifier AST.Decl -> AST.ClassIdentifier -> [AST.FunDecl] -> TInf [AST.FunDecl]
                    rewriteClassFunDecls' _ _ [] = return []
                    rewriteClassFunDecls' declMap i (d : ds) = do
                        ds' <- rewriteClassFunDecls' declMap i ds
                        let (Left i'@(_, m'')) = declIdentifier d
                        let newIdentifier = (AST.Identifier $ classIDName i ++ "." ++ idName i', m'')
                        let (Just (AST.DeclF d', _)) = Map.lookup newIdentifier declMap
                        let d'' = case d' of
                                (AST.FunDeclTyped _ is t ss, m''') -> (AST.FunDeclTyped i' is t ss, m''')
                        return $ d'' : ds'
            rewriteClassDecls' spl (d:ds) = do
                ds' <- rewriteClassDecls' spl ds
                return $ d : ds'
        removeGlobalScopeClassDecls :: AST.SPL -> TInf AST.SPL
        removeGlobalScopeClassDecls [] = return []
        removeGlobalScopeClassDecls (d@(AST.DeclC _, _):ds) = do
            ds' <- removeGlobalScopeClassDecls ds
            return $ d : ds'
        removeGlobalScopeClassDecls (d:ds) = do
            ds' <- removeGlobalScopeClassDecls ds
            let (Left s) = declIdentifier d
            clss <- getDeclClass s
            case clss of
                Just _ -> return ds' -- Remove declarations belonging to a class
                _ -> return $ d : ds'

-- |Find the graph of (global) dependencies; a list of tuples of declarations, identifiers of those declarations,
-- and the (global) identifiers those declarations depend on
tInfSPLGraph :: AST.SPL -> [(AST.Decl, String, [String])]
tInfSPLGraph decls =
    let globalVars = map declIdentifierString decls in
        map (\decl -> (decl, declIdentifierString decl, snd $ dependencies globalVars decl)) decls

tInfDecl :: Type -> AST.Decl -> TInf (AST.Decl, String)
tInfDecl t (AST.DeclC decl, m) = do
    metaMGU m t
    (decl', varName) <- tInfClassDecl t decl
    return ((AST.DeclC decl', m), varName)
tInfDecl t (AST.DeclV decl, m) = do
    metaMGU m t
    (decl', varName) <- tInfVarDecl t decl
    return ((AST.DeclV decl', m), varName)
tInfDecl t (AST.DeclF decl, m) = do
    metaMGU m t
    (decl', varName) <- tInfFunDecl t decl
    return ((AST.DeclF decl', m), varName)

tInfClassDecl :: Type -> AST.ClassDecl -> TInf (AST.ClassDecl, String)
tInfClassDecl t decl@(AST.ClassDecl i vs fs, m) = do
    mgu m t TType
    return (decl, classIDName i)

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
            (ast, str) <- tInfVarDeclUninitialized' m t identifier
            t' <- substitute t
            s' <- mgu m annotatedT t' `catchError` (\_ ->
                throwError $ TInfError (TInfErrorExpectedTypeUnify annotatedT t') (AST.metaPos m))
            if apply s' annotatedT == applyOnlyRename s' annotatedT
                then return (rewrite s' ast, str)
                else throwError $ TInfError (TInfErrorExpectedTypeTooGeneral annotatedT t') (AST.metaPos m)
        (AST.VarDeclUntypedUnitialized identifier, m) -> tInfVarDeclUninitialized' m t identifier
    where
        tInfVarDecl' :: AST.Meta -> Type -> AST.Identifier -> AST.Expression -> TInf (AST.VarDecl, String)
        tInfVarDecl' m t identifier@(_, m') expr = do
            metaMGU m t
            metaMGU m' t
            tInfExpr t expr
            t' <- substitute t
            let t'' = translateType m t'
            return ((AST.VarDeclTyped t'' identifier expr, m), idName identifier)
        tInfVarDeclUninitialized' :: AST.Meta-> Type -> AST.Identifier -> TInf (AST.VarDecl, String)
        tInfVarDeclUninitialized' m t identifier@(_, m') = do
            metaMGU m t
            metaMGU m' t
            let t' = translateType m t
            return ((AST.VarDeclTypedUnitialized t' identifier, m), idName identifier)

tInfFunDecl :: Type -> AST.FunDecl -> TInf (AST.FunDecl, String)
tInfFunDecl t decl =
    case decl of
        (AST.FunDeclTyped identifier args annotatedType stmts, m) -> do
            --let annotatedT = rTranslateType annotatedType
            --(ast, str) <- tInfFunDecl' m t identifier args stmts
            --t' <- substitute t
            --s' <- mgu m annotatedT t' `catchError` (\_ ->
            --    throwError $ TInfError (TInfErrorExpectedTypeUnify annotatedT t') (AST.metaPos m))
            --if apply s' annotatedT == applyOnlyRename s' annotatedT
            --    then return (rewrite s' ast, str)
            --    else throwError $ TInfError (TInfErrorExpectedTypeTooGeneral annotatedT t') (AST.metaPos m)
            let annotatedT = rTranslateType annotatedType
            mgu m t annotatedT
            tInfFunDecl' m annotatedT identifier args stmts
        (AST.FunDeclUntyped identifier args stmts, m) -> tInfFunDecl' m t identifier args stmts
    where
        tInfFunDecl' :: AST.Meta -> Type -> AST.Identifier -> [AST.Identifier] -> [AST.Statement] -> TInf (AST.FunDecl, String)
        tInfFunDecl' m t identifier args stmts = do
            ctx <- ask
            let newCtx = Stack.stackPush ctx emptyCtx
            scopedCtx <- addArgsToCtx (idName identifier ++ "_") newCtx args -- Create the function's scoped context
            local (const scopedCtx) (do
                t' <- newTypeVar "return"
                argsTypes <- getArgsTypes args
                let funType = TFunction argsTypes t'
                mgu m t funType

                (stmts', _) <- tInfStatements t' stmts
                --argsTypes <- getArgsTypes args
                --t'' <- substitute t'
                --let funType = TFunction argsTypes t''
                --mgu m t funType
                --funType' <- substitute funType
                let funTypeAST = translateType m funType

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
                getArgsTypes (arg:args) = do
                    t <- newTypeVar "arg"
                    tInfId t arg
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
    metaMGU m t
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
    t' <- substitute t
    --case expr1 of
    --    (AST.ExprClassMember _ (AST.Identifier "p", _), m) -> throwError $ (TInfError (TInfErrorGeneric $ "asdddd" ++ show t' ++ show expr2) (AST.metaPos m))
    --    _ -> return ()
    return ((AST.StmtAssignment expr1 expr2, m), "", False)
tInfStatement t (AST.StmtFunCall identifier expressions, m) = do
    tInfExpr t (AST.ExprFunCall identifier expressions, m)
    return ((AST.StmtFunCall identifier expressions, m), "", False)
tInfStatement t (AST.StmtReturn expr, m) = do
    tInfExpr t expr
    return ((AST.StmtReturn expr, m), "", True)
tInfStatement t (AST.StmtReturnVoid, m) = return ((AST.StmtReturnVoid, m), "", True)
tInfStatement t (AST.StmtDelete expr, m) = do
    t' <- newTypeVar "a"
    tInfExpr (TPointer t') expr
    mgu m t TVoid
    return ((AST.StmtDelete expr, m), "", False)

------------------------------------------------------------------------------------------------------------------------

class DeclIdentifier a where
    declIdentifier :: a -> Either AST.Identifier AST.ClassIdentifier

declIdentifierString :: DeclIdentifier a => a -> String
declIdentifierString d = case declIdentifier d of
    Left i -> idName i
    Right i -> classIDName i

instance DeclIdentifier AST.Decl where
    declIdentifier (AST.DeclC decl, _) = declIdentifier decl
    declIdentifier (AST.DeclV decl, _) = declIdentifier decl
    declIdentifier (AST.DeclF decl, _) = declIdentifier decl

instance DeclIdentifier AST.ClassDecl where
    declIdentifier (AST.ClassDecl i _ _, _) = Right i

instance DeclIdentifier AST.VarDecl where
    declIdentifier (AST.VarDeclTyped _ i _, _) = Left i
    declIdentifier (AST.VarDeclUntyped i _, _) = Left i
    declIdentifier (AST.VarDeclTypedUnitialized _ i, _) = Left i
    declIdentifier (AST.VarDeclUntypedUnitialized i, _) = Left i

instance DeclIdentifier AST.FunDecl where
    declIdentifier (AST.FunDeclTyped i _ _ _, _) = Left i
    declIdentifier (AST.FunDeclUntyped i _ _, _) = Left i

class Dependencies a where
    dependencies :: [String] -> a -> ([String], [String]) -- tuple of global defs remaining and dependencies

instance Dependencies a => Dependencies [a] where
    dependencies globalDefs [] = (globalDefs, [])
    dependencies globalDefs (a:aa) =
        let (globalDefs', deps) = dependencies globalDefs a in
            let (globalDefs'', deps') = dependencies globalDefs' aa in
                (globalDefs'', deps ++ deps')

instance Dependencies AST.Type where
    dependencies globalDefs t = dependencies globalDefs (rTranslateType t)

instance Dependencies Type where
    dependencies globalDefs (TVar _) = (globalDefs, [])
    dependencies globalDefs TBool = (globalDefs, [])
    dependencies globalDefs TInt = (globalDefs, [])
    dependencies globalDefs TChar = (globalDefs, [])
    dependencies globalDefs (TList t) = dependencies globalDefs t
    dependencies globalDefs (TTuple t1 t2 ) =
        let (_, deps) = dependencies globalDefs t1 in
            let (_, deps') = dependencies globalDefs t2 in
                (globalDefs, deps ++ deps')
    dependencies globalDefs (TFunction ts t) =
        let (_, deps) = dependencies globalDefs ts in
            let (_, deps') = dependencies globalDefs t in
                (globalDefs, deps ++ deps')
    dependencies globalDefs (TPointer t) = dependencies globalDefs t
    dependencies globalDefs TVoid = (globalDefs, [])
    dependencies globalDefs TType = (globalDefs, [])
    dependencies globalDefs (TClass t) = dependencies globalDefs t
    dependencies globalDefs (TClassIdentifier s) = (globalDefs, [s])

instance Dependencies AST.Decl where
    dependencies globalDefs (AST.DeclC decl, _) = dependencies globalDefs decl
    dependencies globalDefs (AST.DeclV decl, _) = dependencies globalDefs decl
    dependencies globalDefs (AST.DeclF decl, _) = dependencies globalDefs decl

instance Dependencies AST.ClassDecl where
    dependencies globalDefs (AST.ClassDecl i vs fs, _) =
        let (_, deps) = dependencies globalDefs vs in
            let (_, deps') = dependencies globalDefs fs in
                (globalDefs, deps ++ deps')

instance Dependencies AST.VarDecl where
    dependencies globalDefs (AST.VarDeclTyped t i e, _) =
        let (_, deps) = dependencies globalDefs t in
            let (globalDefs', deps') = dependencies globalDefs e in
              ([g | g <- globalDefs', g /= idName i], deps ++ deps' ++ dependenciesClass (idName i))
    dependencies globalDefs (AST.VarDeclUntyped i e, _) =
        let (globalDefs', deps) = dependencies globalDefs e in
          ([g | g <- globalDefs, g /= idName i], deps ++ dependenciesClass (idName i))
    dependencies globalDefs (AST.VarDeclTypedUnitialized t i, _) =
        let (_, deps) = dependencies globalDefs t in
            ([g | g <- globalDefs, g /= idName i], deps ++ dependenciesClass (idName i))
    dependencies globalDefs (AST.VarDeclUntypedUnitialized i, _) = ([g | g <- globalDefs, g /= idName i], dependenciesClass (idName i))

instance Dependencies AST.FunDecl where
    dependencies globalDefs (AST.FunDeclTyped i is t ss, _) =
        let (_, deps) = dependencies globalDefs t in
            let (globalDefs', deps') = dependencies [g | g <- globalDefs, g `notElem` map idName is] ss in
                    ([g | g <- globalDefs', g /= idName i], deps ++ deps' ++ dependenciesClass (idName i))
    dependencies globalDefs (AST.FunDeclUntyped i is ss, _) =
        let (globalDefs', deps) = dependencies [g | g <- globalDefs, g `notElem` map idName is] ss in
            ([g | g <- globalDefs', g /= idName i], deps ++ dependenciesClass (idName i))

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
    dependencies globalDefs (AST.StmtDelete e, _) = dependencies globalDefs e

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
    dependencies globalDefs (AST.ExprClassConstructor i es, _) =
        let (_, deps) = dependencies globalDefs es in
            (globalDefs, deps ++ [classIDName i])
    dependencies globalDefs (AST.ExprNew e, _) = dependencies globalDefs e
    dependencies globalDefs (AST.ExprDelete e, _) = dependencies globalDefs e
    dependencies globalDefs (AST.ExprClassMember e i, _) = dependencies globalDefs e

instance Dependencies AST.Identifier where
    dependencies globalDefs (AST.Identifier i, _) =
        if i `elem` globalDefs
            then (globalDefs, i : dependenciesClass i)
            else (globalDefs, dependenciesClass i)

instance Dependencies AST.ClassIdentifier where
    dependencies globalDefs (AST.ClassIdentifier i, _) = (globalDefs, [i])

dependenciesClass :: String -> [String]
dependenciesClass i = if Char.isUpper (head i)
        then [takeWhile (/= '.') i]
        else []

------------------------------------------------------------------------------------------------------------------------

translateType :: AST.Meta -> Type -> AST.Type
translateType m (TVar str)                      = (AST.TypeIdentifier (AST.Identifier str, m), m)
translateType m TVoid                           = (AST.TypeVoid, m)
translateType m TBool                           = (AST.TypeBool, m)
translateType m TInt                            = (AST.TypeInt, m)
translateType m TChar                           = (AST.TypeChar, m)
translateType m (TList t)                       = (AST.TypeList $ translateType m t, m)
translateType m (TTuple t1 t2)                  = (AST.TypeTuple (translateType m t1) (translateType m t2), m)
translateType m (TPointer t)                    = (AST.TypePointer $ translateType m t, m)
translateType m (TFunction args t)              = (AST.TypeFunction (map (translateType m) args) (translateType m t), m)
translateType m TType                           = (AST.TypeType, m)
translateType m (TClass t)                      = (AST.TypeClass (AST.ClassIdentifier $ case t of
                                                        TClassIdentifier str -> str
                                                        TVar str -> str
                                                        , m), m)
translateType m (TClassIdentifier str)          = (AST.TypeClass (AST.ClassIdentifier str, m), m) -- Not a true translation

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
rTranslateType (AST.TypeType, _)            = TType
rTranslateType (AST.TypeClass i, _)         = TClass $ TClassIdentifier $ classIDName i

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
    rewrite s d@(AST.DeclI _, _) = d
    rewrite s (AST.DeclC c, m) = (AST.DeclC (rewrite s c), m)
    rewrite s (AST.DeclV v, m) = (AST.DeclV (rewrite s v), m)
    rewrite s (AST.DeclF f, m) = (AST.DeclF (rewrite s f), m)

    mapMeta f (AST.DeclI i, m) = do
        m' <- f m
        return (AST.DeclI i, m')
    mapMeta f (AST.DeclC c, m) = do
        m' <- f m
        c' <- mapMeta f c
        return (AST.DeclC c', m')
    mapMeta f (AST.DeclV v, m) = do
        m' <- f m
        v' <- mapMeta f v
        return (AST.DeclV v', m')
    mapMeta f (AST.DeclF fun, m) = do
        m' <- f m
        fun' <- mapMeta f fun
        return (AST.DeclF fun', m')

instance RewriteAST AST.ClassDecl where
    rewrite s (AST.ClassDecl i vs fs, m) = (AST.ClassDecl (rewrite s i) (rewrite s vs) (rewrite s fs), m)

    mapMeta f (AST.ClassDecl i vs fs, m) = do
        m' <- f m
        i' <- mapMeta f i
        vs' <- mapMeta f vs
        fs' <- mapMeta f fs
        return (AST.ClassDecl i' vs' fs', m')

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
    rewrite s (AST.StmtDelete e, m) = (AST.StmtDelete (rewrite s e), m)
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
    mapMeta f (AST.StmtReturnVoid, m) = do
        m' <- f m
        return (AST.StmtReturnVoid, m')
    mapMeta f (AST.StmtDelete e, m) = do
        m' <- f m
        e' <- mapMeta f e
        return (AST.StmtDelete e', m')

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
    mapMeta f (AST.ExprClassConstructor i es, m) = do
        m' <- f m
        i' <- mapMeta f i
        es' <- mapMeta f es
        return (AST.ExprClassConstructor i' es', m')
    mapMeta f (AST.ExprNew e, m) = do
        m' <- f m
        e' <- mapMeta f e
        return (AST.ExprNew e', m')
    mapMeta f (AST.ExprDelete e, m) = do
        m' <- f m
        e' <- mapMeta f e
        return (AST.ExprDelete e', m')
    mapMeta f (AST.ExprClassMember e i, m) = do
        m' <- f m
        e' <- mapMeta f e
        i' <- mapMeta f i
        return (AST.ExprClassMember e' i', m')

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

instance RewriteAST AST.ClassIdentifier where
    rewrite _ i = i

    mapMeta f (i, m) = f m >>= \m' -> return (i, m')
