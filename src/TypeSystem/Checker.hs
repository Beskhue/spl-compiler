module TypeSystem.Checker where

import qualified Data.List as List
import Control.Monad.Trans
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Except

import qualified Data.Pos as Pos
import qualified Data.AST as AST

data Type = TBool
          | TInt
          | TChar
          | TList Type
          | TTuple Type Type
          | TFunction Type Type
            deriving (Show, Eq)

data Namespace = Variable
               | Function
                 deriving (Eq)

type Context = [(String, Maybe Type)]
type VariableContext = Context
type FunctionContext = Context

type NamespaceContexts = (VariableContext, FunctionContext)

ctx :: NamespaceContexts
ctx = ([("b", Just TBool), ("i", Just TInt), ("i2", Just TInt)], [])

getVarCtx :: NamespaceContexts -> Context
getVarCtx = fst

getFunCtx :: NamespaceContexts -> Context
getFunCtx = snd

inCtx :: String -> Context -> Bool
inCtx str = any (\(s, _) -> s == str)

notInCtx s ctx = not (inCtx s ctx)

validCtx :: Context -> Bool
validCtx ctx = length ctx == (length $ List.nub $ map fst ctx)

data TypingError = UnexpectedType Type
                   deriving (Eq)
type TypingErrorP = (TypingError, Pos.Pos)

instance Show TypingError where
    show (UnexpectedType t ) = "Unexpected type " ++ show t ++ "."

{-|
Define type checking with a state monad stacked with an exception monad
See http://hackage.haskell.org/package/transformers-0.5.4.0/docs/Control-Monad-Trans-Class.html

A state contains (Current declaration being type checked, Remaining syntax tree)
-}
type TypingT = StateT (AST.Decl, AST.SPL) (Except TypingErrorP)

-- |Throw a parse error
throwTypingError :: TypingError -> Pos.Pos -> TypingT ()
throwTypingError tError pos =
    lift $ throwE (tError, pos)

-- |Advance the state to the next state (i.e., set the declaration as the declaration)
advance :: TypingT ()
advance = do
    (_, x:xs) <- get
    put (x, xs)

-- |Get the current declaration
getDecl :: TypingT AST.Decl
getDecl = do
    (d, _) <- get
    return d

-- |Type check an SPL AST to either a type error or a boolean indicating type success
check :: AST.SPL -> Either TypingErrorP Bool
check ast = runExcept $ evalStateT (advance >> tChAST) (undefined, ast)

-- |Type check a (global) declaration
tChAST :: TypingT Bool
tChAST = do
    (decl, p) <- getDecl
    case decl of
        AST.DeclV varDecl -> return False
        AST.DeclF funDecl -> return False
