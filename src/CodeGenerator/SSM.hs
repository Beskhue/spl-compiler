{-# OPTIONS_GHC -XFlexibleInstances #-}

{-|
Module: CodeGenerator.SSM
Description: Generate code for a SSM from an SPL AST
Copyright: (c) Thomas Churchman, 2017
License: MIT
Maintainer: thomas@kepow.org
Stability: experimental

Generate code for a simple stack machine
http://www.staff.science.uu.nl/~dijks106/SSM/

-}

module CodeGenerator.SSM where

import qualified Debug.Trace as Trace

import qualified Data.Map as Map

import Control.Monad
import Control.Monad.Except
import Control.Monad.State
-- import Data.Foldable (foldrM, foldlM)

import qualified Data.Char as Char

import qualified TypeSystem.Checker as Checker

import qualified Data.Pos as Pos
import qualified Data.AST as AST

--------------------------------------------------------------------------------

class Display a where
    display :: a -> String

instance Display String where
    display l = l

type SSM = [SSMLine]

instance Display SSM where
    display = concatMap display

data SSMLine = SSMLine (Maybe SSMLabel) (Maybe SSMInstruction) (Maybe SSMComment)
               deriving (Show)

instance Display SSMLine where
    display (SSMLine l i c) =
        displayLabel l
        ++ displayInstruction i
        ++ displayComment c
        ++ "\n"
        where
            displayLabel :: Maybe SSMLabel -> String
            displayLabel (Just l) = display l ++ ": "
            displayLabel _ = ""
            displayInstruction :: Maybe SSMInstruction -> String
            displayInstruction (Just i) = display i
            displayInstruction _ = ""
            displayComment :: Maybe SSMComment -> String
            displayComment (Just c) = "; " ++ display c
            displayComment _ = ""

type SSMLabel = String
type SSMComment = String

data SSMInstruction = ILoad SSMLoad
                    | IStore SSMStore
                    | ICompute SSMOperation
                    | IControl SSMControl
                    | IIO SSMIO
                    | IAnnotate SSMArgument SSMArgument SSMArgument SSMArgument SSMArgument
                      deriving (Show, Eq)

instance Display SSMInstruction where
    display (ILoad l) = display l
    display (IStore s) = display s
    display (ICompute o) = display o
    display (IControl c) = display c
    display (IIO io) = display io
    display (IAnnotate arg1 arg2 arg3 arg4 arg5) = "annotate"
        ++ " " ++ display arg1 ++ " " ++ display arg2 ++ " " ++ display arg3
        ++ " " ++ display arg4 ++ " " ++ display arg5

data SSMLoad = LConstant SSMArgument
             | LStack SSMArgument
             | LHeap SSMArgument
             | LMark SSMArgument
             | LAddress SSMArgument
             | LRegister SSMArgument
             | LRegisterFromRegister SSMArgument SSMArgument
             | LStackAddress SSMArgument
             | LMarkAddress SSMArgument
             | LAddressAddress SSMArgument
               deriving (Show, Eq)

instance Display SSMLoad where
    display (LConstant arg) = "ldc " ++ display arg
    display (LStack arg) = "lds " ++ display arg
    display (LHeap arg) = "ldh " ++ display arg
    display (LMark arg) = "ldl " ++ display arg
    display (LAddress arg) = "lda " ++ display arg
    display (LRegister arg) = "ldr " ++ display arg
    display (LRegisterFromRegister arg1 arg2) = "ldrr " ++ display arg1 ++ " " ++ display arg2
    display (LStackAddress arg) = "ldsa " ++ display arg
    display (LMarkAddress arg) = "ldla " ++ display arg
    display (LAddressAddress arg) = "ldaa " ++ display arg

data SSMStore = SStack SSMArgument
              | SHeap
              | SMark SSMArgument
              | SAddress SSMArgument
              | SRegister SSMArgument
                deriving (Show, Eq)

instance Display SSMStore where
    display (SStack arg) = "sts " ++ display arg
    display SHeap = "sth"
    display (SMark arg) = "stl " ++ display arg
    display (SAddress arg) = "sta " ++ display arg
    display (SRegister arg) = "str " ++ display arg

data SSMArgument = ALabel SSMLabel
                 | ARegister SSMRegister
                 | ANumber Int
                 | AText String
                 | AColor String
                   deriving (Show, Eq)

instance Display SSMArgument where
    display (ALabel l) = display l
    display (ARegister r) = display r
    display (ANumber n) = show n
    display (AText s) = s
    display (AColor s) = s

data SSMRegister = RPC | RSP | RMP | RHP | RRR | R5 | R6 | R7
                   deriving (Show, Eq)

instance Display SSMRegister where
    display RPC = "PC"
    display RSP = "SP"
    display RMP = "MP"
    display RHP = "HP"
    display RRR = "RR"
    display R5 = "R5"
    display R6 = "R6"
    display R7 = "R7"

data SSMOperation = OAdd | OSub | OMul | ODiv | OMod
                  | ONeg
                  | OAnd | OOr | OXor
                  | ONot
                  | OCmp
                  | OEq | ONeq
                  | OLt | OGt | OLe | OGe
                    deriving (Show, Eq)

instance Display SSMOperation where
    display OAdd = "add"
    display OSub = "sub"
    display OMul = "mul"
    display ODiv = "div"
    display OMod = "mod"
    display ONeg = "neg"
    display OAnd = "and"
    display OOr = "or"
    display OXor = "xor"
    display ONot = "not"
    display OCmp = "cmp"
    display OEq = "eq"
    display ONeq = "ne"
    display OLt = "lt"
    display OGt = "gt"
    display OLe = "le"
    display OGe = "ge"

data SSMControl = CBranchEq SSMArgument | CBranchNeq SSMArgument
                | CBranchLt SSMArgument | CBranchGt SSMArgument
                | CBranchLe SSMArgument | CBranchGe SSMArgument -- these six might no longer be implement in SSM
                | CBranchAlways SSMArgument | CBranchFalse SSMArgument | CBranchTrue SSMArgument
                | CBranchSubroutine SSMArgument
                | CJumpSubroutine
                | CReturn
                | CLink SSMArgument | CUnlink
                | CAdjustSP SSMArgument
                | CNop | CHalt
                  deriving (Show, Eq)

instance Display SSMControl where
    display (CBranchEq arg) = "beq " ++ display arg
    display (CBranchNeq arg) = "bne " ++ display arg
    display (CBranchLt arg) = "blt " ++ display arg
    display (CBranchGt arg) = "bgt " ++ display arg
    display (CBranchLe arg) = "ble " ++ display arg
    display (CBranchGe arg) = "bge " ++ display arg
    display (CBranchAlways arg) = "bra " ++ display arg
    display (CBranchFalse arg) = "brf " ++ display arg
    display (CBranchTrue arg) = "brt " ++ display arg
    display (CBranchSubroutine arg) = "bsr " ++ display arg
    display CJumpSubroutine = "jsr"
    display CReturn = "ret"
    display (CLink arg) = "link " ++ display arg
    display CUnlink = "unlink"
    display (CAdjustSP arg) = "ajs " ++ display arg
    display CNop = "nop"
    display CHalt = "halt"

data SSMIO = IOPrintInt | IOPrintChar | IOReadInt | IOReadChar
             deriving (Show, Eq)

instance Display SSMIO where
    display IOPrintInt = "trap 0"
    display IOPrintChar = "trap 1"
    display IOReadInt = "trap 10"
    display IOReadChar = "trap 11"

--------------------------------------------------------------------------------

data GenState = GenState { ssm :: SSM,
                           astAnnotation :: Checker.ASTAnnotation }
                    deriving (Show)

type Gen a = ExceptT String (State GenState) a

runGen :: Gen a -> Checker.ASTAnnotation -> (Either String a, GenState)
runGen g astAnnotation = runState (runExceptT g) initGenState
    where
        initGenState = GenState {ssm = [], astAnnotation = astAnnotation}

getASTAnnotation :: Gen Checker.ASTAnnotation
getASTAnnotation = do
    st <- get
    return $ astAnnotation st

push :: SSMLine -> Gen ()
push l = do
    st <- get
    put st {ssm = ssm st ++ [l]}

--------------------------------------------------------------------------------

gen :: Checker.ASTAnnotation -> AST.SPL -> Either String SSM
gen annotation ast  = res
    where
        (res, _) = runGen (genSPL ast) annotation

genDet :: Checker.ASTAnnotation -> AST.SPL -> SSM
genDet annotation spl =
    case gen annotation spl of
        Left err -> Trace.trace (show err) undefined
        Right ssm -> ssm

--------------------------------------------------------------------------------

genSPL :: AST.SPL -> Gen SSM
genSPL decls = do
    -- First add a statement to branch to the main function
    push $ SSMLine Nothing (Just $ IControl $ CBranchAlways $ ALabel "main") Nothing
    -- Process all declarations
    liftM id (mapM genDecl decls)
    -- Output generated SSM
    st <- get
    return $ ssm st

genDecl :: AST.Decl -> Gen ()
genDecl (AST.DeclF funDecl, _) = genFunDecl funDecl

genFunDecl :: AST.FunDecl -> Gen ()
genFunDecl (AST.FunDeclTyped i args _ stmts, _) = do
        push $ SSMLine
            (Just $ Checker.idName i)
            (Just $ IControl $ CLink $ ANumber $ length args)
            Nothing
        liftM id (mapM genStatement stmts)
        if Checker.idName i == "main"
            then push $ SSMLine Nothing (Just $ IControl CHalt) Nothing -- halt
            else do
                push $ SSMLine Nothing (Just $ IControl CUnlink) Nothing
                push $ SSMLine Nothing (Just $ IControl CReturn) Nothing

genStatement :: AST.Statement -> Gen ()
genStatement (AST.StmtFunCall i args, p) = genExpression (AST.ExprFunCall i args, p)

genExpression :: AST.Expression -> Gen ()
genExpression (AST.ExprFunCall i args, p) = do
    a <- getASTAnnotation
    liftM id (mapM genExpression args)
    case Checker.idName i of
        "print" -> case Map.lookup p a of
            Just (Checker.TFunction [Checker.TInt] _) -> push $ SSMLine Nothing (Just $ IIO IOPrintInt) Nothing
            Just (Checker.TFunction [Checker.TChar] _) -> push $ SSMLine Nothing (Just $ IIO IOPrintChar) Nothing
            t -> throwError $ "No overloaded version of print with this type: " ++ show t
        _ -> do
            -- Jump to function
            push $ SSMLine Nothing (Just $ IControl $ CBranchSubroutine $ ALabel $ Checker.idName i) Nothing
            -- Decrement stack pointer
            -- push $ SSMLine Nothing (Just $ IControl $ CAdjustSP $ ANumber $ -1) Nothing
genExpression (AST.ExprConstant c, _) = genConstant c
genExpression (AST.ExprUnaryOp op e, _) = do
    genExpression e
    genUnaryOp op
genExpression (AST.ExprBinaryOp op e1@(_, p1) e2@(_, p2), _) = do
    a <- getASTAnnotation
    case Map.lookup p1 a of
        Just t1 ->
            case Map.lookup p2 a of
                Just t2 ->  do
                    genExpression e1
                    genExpression e2
                    genBinaryOp op t1 t2

genConstant :: AST.Constant -> Gen ()
genConstant (AST.ConstInt i, _) = push $ SSMLine Nothing (Just $ ILoad $ LConstant $ ANumber i) Nothing
genConstant (AST.ConstChar c, _) = push $ SSMLine Nothing (Just $ ILoad $ LConstant $ ANumber $ Char.digitToInt c) Nothing
genConstant (AST.ConstBool b, _) = let n = if b then -1 else 0 in
    push $ SSMLine Nothing (Just $ ILoad $ LConstant $ ANumber $ -1) Nothing
genConstant (AST.ConstEmptyList, _) = do
        push $ SSMLine Nothing (Just $ ILoad $ LConstant $ ANumber $ -1) Nothing
        push $ SSMLine Nothing (Just $ IStore SHeap) Nothing
        push $ SSMLine Nothing (Just $ ILoad $ LConstant $ ANumber $ -1) Nothing
        push $ SSMLine Nothing (Just $ IStore SHeap) Nothing

genUnaryOp :: AST.UnaryOperator -> Gen ()
genUnaryOp (AST.UnaryOpNeg, _) = push $ SSMLine Nothing (Just $ ICompute ONot) Nothing
genUnaryOp (AST.UnaryOpSubtr, _) = push $ SSMLine Nothing (Just $ ICompute ONeg) Nothing

genBinaryOp :: AST.BinaryOperator -> Checker.Type -> Checker.Type -> Gen ()
genBinaryOp (AST.BinaryOpOr, _) _ _ = push $ SSMLine Nothing (Just $ ICompute OOr) Nothing
genBinaryOp (AST.BinaryOpAnd, _) _ _ = push $ SSMLine Nothing (Just $ ICompute OAnd) Nothing
genBinaryOp (AST.BinaryOpEq, _) Checker.TBool Checker.TBool = push $ SSMLine Nothing (Just $ ICompute OEq) Nothing
genBinaryOp (AST.BinaryOpEq, _) _ _ = do -- address, char or int equals
    push $ SSMLine Nothing (Just $ ICompute OSub) Nothing
    push $ SSMLine Nothing (Just $ ICompute OEq) Nothing
genBinaryOp (AST.BinaryOpNEq, p) t1 t2 = do
    genBinaryOp (AST.BinaryOpEq, p) t1 t2
    genUnaryOp (AST.UnaryOpNeg, p) -- Todo: make more efficient
genBinaryOp (AST.BinaryOpLT, _) _ _ = push $ SSMLine Nothing (Just $ ICompute OLt) Nothing
genBinaryOp (AST.BinaryOpGT, _) _ _ = push $ SSMLine Nothing (Just $ ICompute OGt) Nothing
genBinaryOp (AST.BinaryOpLTE, _) _ _ = push $ SSMLine Nothing (Just $ ICompute OLe) Nothing
genBinaryOp (AST.BinaryOpGTE, _) _ _ = push $ SSMLine Nothing (Just $ ICompute OGe) Nothing
genBinaryOp (AST.BinaryOpConcat, _) _ _ = undefined
genBinaryOp (AST.BinaryOpPlus, _) _ _ = push $ SSMLine Nothing (Just $ ICompute OAdd) Nothing
genBinaryOp (AST.BinaryOpSubtr, _) _ _ = push $ SSMLine Nothing (Just $ ICompute OSub) Nothing
genBinaryOp (AST.BinaryOpMult, _) _ _ = push $ SSMLine Nothing (Just $ ICompute OMul) Nothing
genBinaryOp (AST.BinaryOpDiv, _) _ _ = push $ SSMLine Nothing (Just $ ICompute ODiv) Nothing
genBinaryOp (AST.BinaryOpMod, _) _ _ = push $ SSMLine Nothing (Just $ ICompute OMod) Nothing
