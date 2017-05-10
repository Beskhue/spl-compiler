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
import qualified Data.Stack as Stack

import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
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
                 | AChar Char
                 | AText String
                 | AColor String
                   deriving (Show, Eq)

instance Display SSMArgument where
    display (ALabel l) = display l
    display (ARegister r) = display r
    display (ANumber n) = show n
    display (AChar c) = show $ Char.ord c
    display (AText s) = s
    display (AColor s) = s

data SSMRegister = RProgramCounter | RStackPointer | RMarkPointer
                 | RHeapPointer | RReturnRegister
                 | R5 | R6 | R7
                   deriving (Show, Eq)

instance Display SSMRegister where
    display RProgramCounter = "PC"
    display RStackPointer = "SP"
    display RMarkPointer = "MP"
    display RHeapPointer = "HP"
    display RReturnRegister = "RR"
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
                | CAdjustSP SSMArgument | CSwap
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
    display CSwap = "swp"
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

type AddressOffset = Int
data Scope = SGlobal | SLocal
             deriving (Show, Eq)
type VariableScope = Map.Map String AddressOffset
type VariableScopes = Stack.Stack VariableScope

emptyVariableScopes :: VariableScopes
emptyVariableScopes = Stack.stackNew

--------------------------------------------------------------------------------

data GenState = GenState { ssm :: SSM,
                           astAnnotation :: Checker.ASTAnnotation,
                           labelSupply :: Int}
                    deriving (Show)

type Gen a = ExceptT String (ReaderT VariableScopes (State GenState)) a

runGen :: Gen a -> Checker.ASTAnnotation -> (Either String a, GenState)
runGen g astAnnotation = runState (runReaderT (runExceptT g) emptyVariableScopes) initGenState
    where
        initGenState = GenState {ssm = [], astAnnotation = astAnnotation, labelSupply = 0}

getASTAnnotation :: Gen Checker.ASTAnnotation
getASTAnnotation = do
    st <- get
    return $ astAnnotation st

getFreshLabel :: Gen String
getFreshLabel = do
    st <- get
    let i = labelSupply st
    put st {labelSupply = i + 1}
    return $ "__lbl_" ++ show i

push :: SSMLine -> Gen ()
push l = do
    st <- get
    put st {ssm = ssm st ++ [l]}

getVariable :: String -> Gen (Maybe (Scope, AddressOffset))
getVariable s = do
    scopes <- ask
    return $ getVariable' 0 scopes s
    where
        getVariable' :: Int -> VariableScopes -> String -> Maybe (Scope, AddressOffset)
        getVariable' n scopes s = case Stack.stackPop scopes of
            Nothing -> Nothing
            Just (scopes', scope) -> case Map.lookup s scope of
                Nothing -> getVariable' (n + 1) scopes' s
                Just result -> if n == 0
                    then Just (SGlobal, result)
                    else Just (SLocal, result)

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
    -- Process all variable declarations
    let varDecls = filter (\decl -> case decl of
            (AST.DeclV _, _) -> True
            _ -> False) decls
    let varDeclNames = map (\decl -> case decl of (AST.DeclV (AST.VarDeclTyped _ i _, _), _) -> Checker.idName i) varDecls
    scopes <- ask
    let scopes' = Stack.stackPush scopes (Map.fromList [(varDeclName, idx) | (idx, varDeclName) <- zip [0..] varDeclNames])
    local (const scopes') (liftM id (mapM genDecl varDecls))
    -- First add a statement to branch to the main function
    push $ SSMLine Nothing (Just $ IControl $ CBranchSubroutine $ ALabel "main") Nothing
    -- Halt
    push $ SSMLine (Nothing) (Just $ IControl CHalt) Nothing
    -- Process all function declarations
    local (const scopes') (liftM id (mapM genDecl (filter (\decl -> case decl of
        (AST.DeclF _, _) -> True
        _ -> False) decls)))
    -- Add label to end of PC
    push $ SSMLine (Just "__end_pc") (Just $ IControl CNop) Nothing
    -- Output generated SSM
    st <- get
    return $ ssm st

genDecl :: AST.Decl -> Gen ()
genDecl (AST.DeclV varDecl, _) = genVarDecl varDecl
genDecl (AST.DeclF funDecl, _) = genFunDecl funDecl

genVarDecl :: AST.VarDecl -> Gen ()
genVarDecl (AST.VarDeclTyped _ i e, _) = genExpression e

genFunDecl :: AST.FunDecl -> Gen ()
genFunDecl (AST.FunDeclTyped i args _ stmts, _) = do
        push $ SSMLine
            (Just $ Checker.idName i)
            (Just $ IControl $ CLink $ ANumber $ length args)
            Nothing
        liftM id (mapM genStatement stmts)
        push $ SSMLine Nothing (Just $ IControl CUnlink) Nothing
        push $ SSMLine Nothing (Just $ IControl CReturn) Nothing

genStatement :: AST.Statement -> Gen ()
genStatement (AST.StmtFunCall i args, p) = genExpression (AST.ExprFunCall i args, p)
genStatement (AST.StmtReturn e, _) = do
    genExpression e
    push $ SSMLine Nothing (Just $ IStore $ SRegister $ ARegister RReturnRegister) Nothing
    push $ SSMLine Nothing (Just $ IControl CUnlink) Nothing
    push $ SSMLine Nothing (Just $ IControl CReturn) Nothing

genExpression :: AST.Expression -> Gen ()
genExpression (AST.ExprIdentifier i, p) = do
    location <- getVariable $ Checker.idName i
    case location of
        Just (SGlobal, offset) -> do -- Load a global
            -- Load the address of the end of the program code
            push $ SSMLine Nothing (Just $ ILoad $ LConstant $ ALabel "__end_pc") (Just $ "load global " ++ Checker.idName i)
            -- Load the value at the address of (the end of the program code + offset)
            push $ SSMLine Nothing (Just $ ILoad $ LAddress $ ANumber (endPCToStartStackOffset + offset)) (Nothing)
        Just (SLocal, offset) -> return ()
        Nothing -> throwError $ "Variable " ++ Checker.idName i ++ " not in scope"
genExpression (AST.ExprFunCall i args, p) = do
    a <- getASTAnnotation
    liftM id (mapM genExpression args)
    case Checker.idName i of
        "print" -> case Map.lookup p a of Just (Checker.TFunction [t] _) -> genPrint t
        "isEmpty" -> do
            -- Get next address of the list, if it is -1 the list is empty
            push $ SSMLine Nothing (Just $ ILoad $ LHeap $ ANumber 0) (Just "start isEmpty")
            push $ SSMLine Nothing (Just $ ILoad $ LConstant $ ANumber $ -1) Nothing
            push $ SSMLine Nothing (Just $ ICompute OEq) (Just "end isEmpty")
        "length" -> do
            -- Start with 0 length
            push $ SSMLine Nothing (Just $ ILoad $ LConstant $ ANumber 0) (Just "start length")
            push $ SSMLine Nothing (Just $ IControl $ CSwap) Nothing
            -- Copy heap address
            push $ SSMLine Nothing (Just $ IStore $ SStack $ ANumber $ 1) Nothing
            push $ SSMLine Nothing (Just $ IControl $ CAdjustSP $ ANumber $ 2) Nothing
            -- Get next address of the list, if it is -1 the list is empty
            push $ SSMLine Nothing (Just $ ILoad $ LHeap $ ANumber $ -1) Nothing
            push $ SSMLine Nothing (Just $ ILoad $ LConstant $ ANumber $ -1) Nothing
            push $ SSMLine Nothing (Just $ ICompute OEq) Nothing
            -- If the list is empty, skip loop -- jump to clean up
            push $ SSMLine Nothing (Just $ IControl $ CBranchTrue $ ANumber $ 8) Nothing
            -- Otherwise load the next address of the list
            push $ SSMLine Nothing (Just $ ILoad $ LHeap $ ANumber $ -1) Nothing
            -- And increment the counter
            push $ SSMLine Nothing (Just $ IControl $ CSwap) Nothing
            genUtilIncrement
            push $ SSMLine Nothing (Just $ IControl $ CBranchAlways $ ANumber $ -20) Nothing
            -- Clean up
            push $ SSMLine Nothing (Just $ IControl $ CAdjustSP $ ANumber $ -1) (Just "end length")
        _ -> do
            -- Jump to function
            push $ SSMLine Nothing (Just $ IControl $ CBranchSubroutine $ ALabel $ Checker.idName i) Nothing
            -- Load returned value
            push $ SSMLine Nothing (Just $ ILoad $ LRegister $ ARegister RReturnRegister) Nothing
    where
        genPrint :: Checker.Type -> Gen ()
        genPrint Checker.TBool = do
            push $ SSMLine Nothing (Just $ IControl $ CBranchFalse $ ANumber 4) (Just "start print bool")
            push $ SSMLine Nothing (Just $ ILoad $ LConstant $ ANumber 1) Nothing -- True
            push $ SSMLine Nothing (Just $ IControl $ CBranchAlways $ ANumber 2) Nothing
            push $ SSMLine Nothing (Just $ ILoad $ LConstant $ ANumber 0) Nothing -- False
            push $ SSMLine Nothing (Just $ IIO IOPrintInt) (Just "end print bool")
        genPrint Checker.TInt = push $ SSMLine Nothing (Just $ IIO IOPrintInt) Nothing
        genPrint Checker.TChar = push $ SSMLine Nothing (Just $ IIO IOPrintChar) Nothing
        genPrint (Checker.TList a) = do
            lblStart <- getFreshLabel
            lblCleanUp <- getFreshLabel
            genPrintChar '['
            -- Copy heap address
            push $ SSMLine (Just lblStart) (Just $ IStore $ SStack $ ANumber $ 1) Nothing
            push $ SSMLine Nothing (Just $ IControl $ CAdjustSP $ ANumber $ 2) Nothing
            -- Get next address of the list, if it is -1 the list is empty
            push $ SSMLine Nothing (Just $ ILoad $ LHeap $ ANumber $ -1) Nothing
            push $ SSMLine Nothing (Just $ ILoad $ LConstant $ ANumber $ -1) Nothing
            push $ SSMLine Nothing (Just $ ICompute OEq) Nothing
            -- If the list is empty, skip loop -- jump to clean up
            push $ SSMLine Nothing (Just $ IControl $ CBranchTrue $ ALabel lblCleanUp) Nothing
            -- Otherwise load the value of the list, first copy heap address again
            push $ SSMLine (Just lblStart) (Just $ IStore $ SStack $ ANumber $ 1) Nothing
            push $ SSMLine Nothing (Just $ IControl $ CAdjustSP $ ANumber $ 2) Nothing
            -- Load value
            push $ SSMLine Nothing (Just $ ILoad $ LHeap $ ANumber $ 0) Nothing
            -- Recursively print
            genPrint a
            genPrintChar ','
            -- Load the next address of the list
            push $ SSMLine Nothing (Just $ ILoad $ LHeap $ ANumber $ -1) Nothing
            -- Jump back to start of loop
            push $ SSMLine Nothing (Just $ IControl $ CBranchAlways $ ALabel lblStart) Nothing
            -- Clean up
            push $ SSMLine (Just lblCleanUp) (Just $ IControl $ CAdjustSP $ ANumber $ -1) Nothing
            genPrintChar ']'
        genPrint (Checker.TVar _) = return ()

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
        -- Adjust SP to point back
        push $ SSMLine Nothing (Just $ IControl $ CAdjustSP $ ANumber $ -1) Nothing
        push $ SSMLine Nothing (Just $ ILoad $ LConstant $ ANumber $ -1) Nothing
        push $ SSMLine Nothing (Just $ IStore SHeap) Nothing

genUnaryOp :: AST.UnaryOperator -> Gen ()
genUnaryOp (AST.UnaryOpNeg, _) = push $ SSMLine Nothing (Just $ ICompute ONot) Nothing
genUnaryOp (AST.UnaryOpSubtr, _) = push $ SSMLine Nothing (Just $ ICompute ONeg) Nothing

genBinaryOp :: AST.BinaryOperator -> Checker.Type -> Checker.Type -> Gen ()
genBinaryOp (AST.BinaryOpOr, _) _ _ = push $ SSMLine Nothing (Just $ ICompute OOr) Nothing
genBinaryOp (AST.BinaryOpAnd, _) _ _ = push $ SSMLine Nothing (Just $ ICompute OAnd) Nothing
genBinaryOp (AST.BinaryOpEq, _) _ _ = push $ SSMLine Nothing (Just $ ICompute OEq) Nothing
--genBinaryOp (AST.BinaryOpEq, _) Checker.TBool Checker.TBool = push $ SSMLine Nothing (Just $ ICompute OEq) Nothing
--genBinaryOp (AST.BinaryOpEq, _) _ _ = do -- address, char or int equals
--    -- push $ SSMLine Nothing (Just $ ICompute OSub) Nothing
--    push $ SSMLine Nothing (Just $ ICompute OEq) Nothing
genBinaryOp (AST.BinaryOpNEq, p) t1 t2 = do
    genBinaryOp (AST.BinaryOpEq, p) t1 t2
    genUnaryOp (AST.UnaryOpNeg, p) -- Todo: make more efficient
genBinaryOp (AST.BinaryOpLT, _) _ _ = push $ SSMLine Nothing (Just $ ICompute OLt) Nothing
genBinaryOp (AST.BinaryOpGT, _) _ _ = push $ SSMLine Nothing (Just $ ICompute OGt) Nothing
genBinaryOp (AST.BinaryOpLTE, _) _ _ = push $ SSMLine Nothing (Just $ ICompute OLe) Nothing
genBinaryOp (AST.BinaryOpGTE, _) _ _ = push $ SSMLine Nothing (Just $ ICompute OGe) Nothing
genBinaryOp (AST.BinaryOpConcat, _) _ _ = do
    -- Stack points to address of tail; store it on the heap
    push $ SSMLine Nothing (Just $ IStore SHeap) (Just "start concat")
    -- Adjust stack to point to the value we want to add
    push $ SSMLine Nothing (Just $ IControl $ CAdjustSP $ ANumber $ -1) Nothing
    -- Store the value on the stack into the heap
    push $ SSMLine Nothing (Just $ IStore SHeap) (Just "end concat")
    -- TODO: push $ SSMLine Nothing (Just $ IStore SMultipleHeap) (Just "concat")
genBinaryOp (AST.BinaryOpPlus, _) _ _ = push $ SSMLine Nothing (Just $ ICompute OAdd) Nothing
genBinaryOp (AST.BinaryOpSubtr, _) _ _ = push $ SSMLine Nothing (Just $ ICompute OSub) Nothing
genBinaryOp (AST.BinaryOpMult, _) _ _ = push $ SSMLine Nothing (Just $ ICompute OMul) Nothing
genBinaryOp (AST.BinaryOpDiv, _) _ _ = push $ SSMLine Nothing (Just $ ICompute ODiv) Nothing
genBinaryOp (AST.BinaryOpMod, _) _ _ = push $ SSMLine Nothing (Just $ ICompute OMod) Nothing

--------------------------------------------------------------------------------
-- Some utilities

genUtilIncrement :: Gen ()
genUtilIncrement = do -- Code takes 3 bytes
    push $ SSMLine Nothing (Just $ ILoad $ LConstant $ ANumber 1) Nothing
    push $ SSMLine Nothing (Just $ ICompute OAdd) Nothing

genPrintChar :: Char -> Gen ()
genPrintChar c = do
    push $ SSMLine Nothing (Just $ ILoad $ LConstant $ AChar c) Nothing
    push $ SSMLine Nothing (Just $ IIO IOPrintChar) Nothing

endPCToStartStackOffset :: Int
endPCToStartStackOffset = 18
