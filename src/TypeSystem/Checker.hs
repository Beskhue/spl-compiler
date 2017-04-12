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

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State

import qualified Data.Pos as Pos
import qualified Data.AST as AST

------------------------------------------------------------------------------------------------------------------------

-- |Todo: implement main SPL checker
--check :: AST.SPL -> Either String AST.SPL
--check spl = res
--    where
--        (res, _) = runTCheck $ tCheckSPL spl

check :: AST.SPL -> Either String AST.SPL
check spl =
    case res of
        Left err -> Left err
        Right (spl, _, _) -> Right spl
    where
        (res, _) = runTInf $ tInfSPL emptyCtx spl

typeInferenceDet :: (TypeCtx -> a -> TInf (Substitution, Type)) -> Map.Map String Scheme -> a -> (Substitution, Type)
typeInferenceDet tInf' ctx e =
    case typeInference tInf' ctx e of
        Left err -> Trace.trace (show err) undefined
        Right t -> t

onlyType :: Either String (Substitution, Type) -> Either String Type
onlyType res =
    case res of
        Left err -> Left err
        Right (_, t) -> Right t

typeInference :: (TypeCtx -> a -> TInf (Substitution, Type)) -> Map.Map String Scheme -> a -> Either String (Substitution, Type)
typeInference tInf' ctx e = res
    where
        (res, _) = runTInf $ tInf' (TypeCtx ctx) e

typeInferenceExpr :: Map.Map String Scheme -> AST.Expression ->  Either String (Substitution, Type)
typeInferenceExpr = typeInference tInfExpr
--typeInferenceExpr ctx e = do
--    s, t) <- tInfExpr (TypeCtx ctx) e
--    return t

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

instance Types Scheme where
    freeTypeVars (Scheme vars t) = (freeTypeVars t) `Set.difference` (Set.fromList vars)
    apply s (Scheme vars t) = Scheme vars (apply (foldr Map.delete s vars) t)

-- |Might be useful to have the class available on lists as well
instance Types a => Types [a] where
    freeTypeVars l = foldr Set.union Set.empty (map freeTypeVars l)
    apply s = map (apply s)

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

emptyMap :: Map.Map String Scheme
emptyMap = Map.empty

emptyCtx :: TypeCtx
emptyCtx = TypeCtx (emptyMap)

-- |Remove a term variable from the context
remove :: TypeCtx -> String -> TypeCtx
remove (TypeCtx ctx) var = TypeCtx (Map.delete var ctx)

add :: TypeCtx -> String -> Scheme -> TypeCtx
--add (TypeCtx ctx) var scheme = TypeCtx (ctx `Map.union` (Map.singleton var scheme))
add (TypeCtx ctx) var scheme = TypeCtx (Map.insert var scheme ctx)

instance Types TypeCtx where
    freeTypeVars (TypeCtx ctx) = freeTypeVars (Map.elems ctx)
    apply s (TypeCtx ctx) = TypeCtx (Map.map (apply s) ctx)

------------------------------------------------------------------------------------------------------------------------

-- |Abstract a type over all type variables free in the type but not free in the context, i.e. creates a type scheme
-- for a type in a given context, by binding all type variables in the type that are not bound in the context to the
-- type.
generalize :: TypeCtx -> Type -> Scheme
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
    return (TVar ("_" ++ show (tInfSupply s) ++ suffix))

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
mgu (TFunction [] body) (TFunction [] body') = do
    s1 <- mgu body body'
    return s1
mgu (TFunction (arg:args) body) (TFunction (arg':args') body') = do
    s1 <- mgu arg arg'
    s2 <- mgu (apply s1 (TFunction args body)) (apply s1 (TFunction args' body'))
    return $ s1 `composeSubstitution` s2
mgu TVoid TVoid          = return nullSubstitution
mgu t1 t2                = throwError $ "types do not unify: " ++ show t1 ++ " and " ++ show t2

------------------------------------------------------------------------------------------------------------------------

-- |Get the name of an AST identifier
idName :: AST.Identifier -> String
idName (AST.Identifier i, _) = i

getScheme :: TypeCtx -> String -> TInf Scheme
getScheme (TypeCtx ctx) varName =
    case Map.lookup varName ctx of
        Nothing -> throwError $ "unbound variable: " ++ varName
        Just scheme -> return scheme

tInfVarName :: TypeCtx -> String -> TInf Type
tInfVarName (TypeCtx ctx) varName =
    case Map.lookup varName ctx of
        Nothing -> throwError $ "unbound variable: " ++ varName
        Just scheme -> do
            t <- instantiate scheme
            return t

-- |Perform type inference on an AST identifier
tInfId :: TypeCtx -> AST.Identifier -> TInf (Substitution, Type)
tInfId (TypeCtx ctx) (AST.Identifier i, _) =
    case Map.lookup i ctx of
        Nothing -> throwError $ "unbound variable: " ++ i
        Just scheme -> do
            t <- instantiate scheme
            return (nullSubstitution, t)

-- |Perform type inference on an AST constant
tInfConst :: TypeCtx -> AST.Constant -> TInf (Substitution, Type)
tInfConst _ (AST.ConstBool _, _) = return (nullSubstitution, TBool)
tInfConst _ (AST.ConstInt _, _) = return (nullSubstitution, TInt)
tInfConst _ (AST.ConstChar _, _) = return (nullSubstitution, TChar)
tInfConst _ (AST.ConstEmptyList, _) = do
    tVar <- newTypeVar "a"
    return (nullSubstitution, TList tVar)

-- |Perform type inference on an AST expression
tInfExpr :: TypeCtx -> AST.Expression -> TInf (Substitution, Type)
tInfExpr ctx (AST.ExprIdentifier id, _) = tInfId ctx id
tInfExpr ctx (AST.ExprFunCall id args, _) = do
    tReturn <- newTypeVar (idName id ++ "_ret")
    (s1, t1) <- tInfId ctx id
    (s2, ts) <- tInfExprs (apply s1 ctx) args
    s <- mgu (apply s2 t1) (TFunction ts tReturn)
    return (s `composeSubstitution` s2 `composeSubstitution` s1, apply s tReturn)
tInfExpr ctx (AST.ExprConstant const, _) = tInfConst ctx const
tInfExpr ctx (AST.ExprBinaryOp op e1 e2, _) = tInfBinaryOp ctx op e1 e2

-- |Perform type inference on a list of AST expressions
tInfExprs :: TypeCtx -> [AST.Expression] -> TInf (Substitution, [Type])
tInfExprs _ [] = return (nullSubstitution, [])
tInfExprs ctx (expr:exprs) = do
    (s1, t) <- tInfExpr ctx expr
    (s2, ts) <- tInfExprs (apply s1 ctx) exprs

    return (s2 `composeSubstitution` s1, (apply s2 t) : ts)

tInfBinaryOp :: TypeCtx -> AST.BinaryOperator -> AST.Expression -> AST.Expression -> TInf (Substitution, Type)
tInfBinaryOp ctx (AST.BinaryOpEq, _) e1 e2 = do
    (s1, t1) <- tInfExpr ctx e1
    (s2, t2) <- tInfExpr (apply s1 ctx) e2
    s <- mgu (apply s2 t1) t2
    return (s `composeSubstitution` s2 `composeSubstitution` s1, TBool)
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

------------------------------------------------------------------------------------------------------------------------

-- |Perform type inference on the global SPL declarations. Rewrite the AST such that all variables are typed as
-- completely as possible.
tInfSPL :: TypeCtx -> AST.SPL -> TInf (AST.SPL, Substitution, Type)
tInfSPL ctx decls = do
    ctx' <- addGlobalsToCtx ctx decls
    tInfSPL' ctx' decls
    where
        addGlobalsToCtx :: TypeCtx -> AST.SPL -> TInf TypeCtx
        addGlobalsToCtx ctx [] = return ctx
        addGlobalsToCtx ctx (decl:decls) = do
            typeVar <- newTypeVar "global" -- Create a (temporary) new type var for this global
            ctx' <- addGlobalsToCtx ctx decls
            case decl of
                (AST.DeclV (AST.VarDeclTyped _ i _, _), _) -> return $ add ctx' (idName i) (Scheme [] typeVar)
                (AST.DeclV (AST.VarDeclUntyped i _, _), _) -> return $ add ctx' (idName i) (Scheme [] typeVar)
                (AST.DeclF (AST.FunDeclTyped i _ _ _, _), _) -> return $ add ctx' (idName i) (Scheme [] typeVar)
                (AST.DeclF (AST.FunDeclUntyped i _ _, _), _) -> return $ add ctx' (idName i) (Scheme [] typeVar)

        tInfSPL' :: TypeCtx -> AST.SPL -> TInf (AST.SPL, Substitution, Type)
        tInfSPL' _ [] = return ([], nullSubstitution, TVoid)
        tInfSPL' ctx (decl:decls) = do
            -- Infer type of the declaration
            (decl', s1, varName, t1) <- tInfDecl ctx decl

            -- Get the type scheme of the variable/function we just declared and unify it with the actual type
            (Scheme _ t1') <- getScheme ctx varName
            s2 <- mgu t1' t1

            -- Get the next declarations, using the unified type scheme
            (decls', s3, t2) <- tInfSPL' (apply s2 ctx) decls

            -- Infer type for this declaration again, using the "back-flown" information from the next declarations
            (declPost, s1Post, varNamePost, t1Post) <- tInfDecl (apply s3 ctx) decl

            return (
                        declPost : decls',
                        s3 `composeSubstitution` s2,
                        t2
                    )

tInfDecl :: TypeCtx -> AST.Decl -> TInf (AST.Decl, Substitution, String, Type)
tInfDecl ctx (AST.DeclV decl, p) = do
    (decl', s, varName, t) <- tInfVarDecl ctx decl
    return ((AST.DeclV decl', p), s, varName, t)
tInfDecl ctx (AST.DeclF decl, p) = do
    (decl', s, varName, t) <- tInfFunDecl ctx decl
    return ((AST.DeclF decl', p), s, varName, t)

tInfVarDecl :: TypeCtx -> AST.VarDecl -> TInf (AST.VarDecl, Substitution, String, Type)
tInfVarDecl ctx decl =
    case decl of
        (AST.VarDeclTyped _ identifier expr, p) -> tInfVarDecl' p ctx identifier expr -- todo: check annotation (probably by checking whether mgu on translated type and inferred type succeeds)
        (AST.VarDeclUntyped identifier expr, p) -> tInfVarDecl' p ctx identifier expr
    where
        tInfVarDecl' :: Pos.Pos -> TypeCtx -> AST.Identifier -> AST.Expression -> TInf (AST.VarDecl, Substitution, String, Type)
        tInfVarDecl' p ctx identifier expr = do
            (s, t) <- tInfExpr ctx expr
            let t' = translateType p t
            return ((AST.VarDeclTyped t' identifier expr, p), s, idName identifier, t)

tInfFunDecl :: TypeCtx -> AST.FunDecl -> TInf (AST.FunDecl, Substitution, String, Type)
tInfFunDecl ctx decl =
    case decl of
        (AST.FunDeclTyped identifier args _ stmts, p) -> tInfFunDecl' p ctx identifier args stmts -- todo: check annotation
        (AST.FunDeclUntyped identifier args stmts, p) -> tInfFunDecl' p ctx identifier args stmts
    where
        tInfFunDecl' :: Pos.Pos -> TypeCtx -> AST.Identifier -> [AST.Identifier] -> [AST.Statement] -> TInf (AST.FunDecl, Substitution, String, Type)
        tInfFunDecl' p ctx identifier args stmts = do
            scopedCtx <- addArgsToCtx (idName identifier ++ "_") ctx args -- Create the function's scoped context
            (stmts', s, t) <- tInfStatements scopedCtx stmts

            argsTypes <- getArgsTypes (apply s scopedCtx) args
            let funType = TFunction argsTypes t
            let funTypeAST = translateFunType p funType

            return ((AST.FunDeclTyped identifier args funTypeAST stmts', p), s, idName identifier, funType) -- todo calculate function type based on stmts return type and argument types
            where
                addArgsToCtx :: String -> TypeCtx -> [AST.Identifier] -> TInf TypeCtx
                addArgsToCtx prefix ctx [] = return ctx
                addArgsToCtx prefix ctx (arg:args) = do
                    typeVar <- newTypeVar (prefix ++ "arg")
                    ctx' <- addArgsToCtx prefix ctx args
                    return $ add ctx' (idName arg) (Scheme [] typeVar)
                    -- return $ add ctx' (idName arg) (generalize ctx' typeVar)

                getArgsTypes :: TypeCtx -> [AST.Identifier] -> TInf [Type]
                getArgsTypes _ [] = return []
                getArgsTypes ctx (arg:args) = do
                    t <- tInfVarName ctx $ idName arg
                    ts <- getArgsTypes ctx args
                    return $ t:ts


tInfStatements :: TypeCtx -> [AST.Statement] -> TInf ([AST.Statement], Substitution, Type)
tInfStatements _ [] = return ([], nullSubstitution, TVoid)
tInfStatements ctx (statement:statements) = do
    (statement', s1, varName, t1, returnsValue) <- tInfStatement ctx statement

    -- Update local context if the statement declared a new variable
    let ctx' = (if varName == ""
                then apply s1 ctx
                else add (apply s1 ctx) varName (Scheme [] t1))

    (statements', s2, t2) <- tInfStatements ctx' statements

    let s = s2 `composeSubstitution` s1

    case returnsValue of
        True -> (
            case t2 of
                TVoid -> return (apply s $ statement' : statements', s, apply s t1)
                _ -> do
                    s <- mgu (apply s2 t1) t2 -- todo: both the composed adn the mgu substitution are called 's', probably will be buggy
                    return (apply s $ statement' : statements', s, apply s t1)
            )

        False -> return (apply s $ statement' : statements', s, apply s t2)

tInfStatement :: TypeCtx -> AST.Statement -> TInf (AST.Statement, Substitution, String, Type, Bool)
tInfStatement ctx (AST.StmtVarDecl decl, p) = do
    (decl', s, varName, t) <- tInfVarDecl ctx decl
    return ((AST.StmtVarDecl decl', p), s, varName, t, False)
tInfStatement ctx (AST.StmtReturn expr, p) = do
    (s, t) <- tInfExpr ctx expr
    return ((AST.StmtReturn expr, p), s, "", t, True)
tInfStatement ctx (AST.StmtAssignment identifier expr, p) = do
    (Scheme _ t) <- getScheme ctx (idName identifier)
    (s1, t1) <- tInfExpr ctx expr

    s <- mgu t t1

    let t' = translateType p (apply s t1)
    return ((AST.StmtAssignment identifier expr, p), s `composeSubstitution` s1, "", apply s t1, False)

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

instance Types AST.Decl where
    freeTypeVars = undefined
    apply s (AST.DeclV v, p) = (AST.DeclV (apply s v), p)
    apply s (AST.DeclF f, p) = (AST.DeclF (apply s f), p)

instance Types AST.VarDecl where
    freeTypeVars = undefined
    apply s (AST.VarDeclTyped t i e, p) = (AST.VarDeclTyped (apply s t) (apply s i) (apply s e), p)
    apply s (AST.VarDeclUntyped i e, p) = (AST.VarDeclUntyped (apply s i) (apply s e), p)

instance Types AST.FunDecl where
    freeTypeVars = undefined
    apply s (AST.FunDeclTyped i is t ss, p) = (AST.FunDeclTyped (apply s i) (apply s is) (apply s t) (apply s ss), p)
    apply s (AST.FunDeclUntyped i is ss, p) = (AST.FunDeclUntyped (apply s i) (apply s is) (apply s ss), p)

instance Types AST.FunType where
    freeTypeVars = undefined
    apply s (AST.FunType ts t, p) = (AST.FunType (apply s ts) (apply s t), p)
    apply s (AST.FunTypeVoid ts, p) = (AST.FunTypeVoid (apply s ts), p)

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

instance Types AST.Field where
    freeTypeVars = undefined
    apply _ f = f

instance Types AST.Expression where
    freeTypeVars = undefined
    apply _ e = e

instance Types AST.Identifier where
    freeTypeVars = undefined
    apply _ i = i

------------------------------------------------------------------------------------------------------------------------


{-

type Rewrite = Bool
type TCheck a = ExceptT String (State (TypeCtx, Rewrite)) a

-- |The type check runner.
runTCheck :: TCheck a -> (Either String a, (TypeCtx, Rewrite))
runTCheck t = runState (runExceptT t) (emptyCtx, False)

-- |Get the context from the state
getCtx :: TCheck TypeCtx
getCtx = do
    (ctx, _) <- get
    return ctx

getRewrite :: TCheck Rewrite
getRewrite = do
    (_, rewrite) <- get
    return rewrite

-- |Set the type checker to type rewrite mode (second pass)
setRewrite :: TCheck ()
setRewrite = do
    (ctx, _) <- get
    put (ctx, True)

addTermToCtx :: String -> Substitution -> Type -> TCheck ()
addTermToCtx str s t = do
    ctx <- getCtx
    let ctx' = apply s ctx in
        let scheme = generalize ctx' t in do
            r <- getRewrite
            put (add ctx' str scheme, r)

-- |Type checks an SPL program, and returns a new AST that is typed as complete as possible
tCheckSPL :: AST.SPL -> TCheck AST.SPL
tCheckSPL decls = do
    tCheckSPL' decls
    setRewrite
    tCheckSPL' decls
    where
        tCheckSPL' :: AST.SPL -> TCheck AST.SPL
        tCheckSPL' [] = return []
        tCheckSPL' (decl:decls) = do
            decl' <- tCheckDecl decl
            decls' <- tCheckSPL' decls
            return $ decl' : decls'

tCheckDecl :: AST.Decl -> TCheck AST.Decl
tCheckDecl (AST.DeclV v, p) = do
    varDecl <- tCheckVarDecl v
    return $ (AST.DeclV varDecl, p)

tCheckVarDecl :: AST.VarDecl -> TCheck AST.VarDecl
tCheckVarDecl v =
    case v of
        (AST.VarDeclTyped annotatedType identifier expr, p) -> do
            (ast, t) <- tCheckVarDecl' p identifier expr

            let t' = rTranslateType annotatedType in do
                let s = runTInf $ mgu t t' in
                    if (apply s t) == (apply s t')
                        then return ast
                        else throwError $ "Expected type: " ++ show t' ++ ". Actual type: " ++ show t
        (AST.VarDeclUntyped identifier expr, p) -> do
            (ast, _) <- tCheckVarDecl' p identifier expr
            return ast
    where
        tCheckVarDecl' :: Pos.Pos -> AST.Identifier -> AST.Expression -> TCheck (AST.VarDecl, Type)
        tCheckVarDecl' p identifier expr = do
            ctx <- getCtx
            case runTInf $ tInfExpr ctx expr of
                (Left err, _) -> throwError $ err
                (Right (s, t), _) -> do
                    addTermToCtx (idName identifier) s t
                    r <- getRewrite
                    if r
                        then return ((AST.VarDeclTyped (translateType p t) identifier expr, p), t)
                        else return ((AST.VarDeclUntyped identifier expr, p), t)

tCheckFunDecl :: AST.FunDecl -> TCheck AST.FunDecl
tCheckFunDecl = undefined

------------------------------------------------------------------------------------------------------------------------

-}