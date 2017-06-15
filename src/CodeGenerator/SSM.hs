{-# LANGUAGE FlexibleInstances #-}

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

import GHC.Generics

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
import Data.Type as Type

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
             | LHeapMultiple SSMArgument SSMArgument
             | LMark SSMArgument
             | LAddress SSMArgument
             | LLocalAddress SSMArgument
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
    display (LHeapMultiple arg1 arg2) = "ldmh " ++ display arg1 ++ " " ++ display arg2
    display (LMark arg) = "ldl " ++ display arg
    display (LAddress arg) = "lda " ++ display arg
    display (LLocalAddress arg) = "ldla " ++ display arg
    display (LRegister arg) = "ldr " ++ display arg
    display (LRegisterFromRegister arg1 arg2) = "ldrr " ++ display arg1 ++ " " ++ display arg2
    display (LStackAddress arg) = "ldsa " ++ display arg
    display (LMarkAddress arg) = "ldla " ++ display arg
    display (LAddressAddress arg) = "ldaa " ++ display arg

data SSMStore = SStack SSMArgument
              | SHeap
              | SHeapMultiple SSMArgument
              | SMark SSMArgument
              | SAddress SSMArgument
              | SRegister SSMArgument
                deriving (Show, Eq)

instance Display SSMStore where
    display (SStack arg) = "sts " ++ display arg
    display SHeap = "sth"
    display (SHeapMultiple arg) = "stmh " ++ display arg
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
                  | OLsl | OLsr
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
    display OLsl = "lsl"
    display OLsr = "lsr"

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

emptyVariableScope :: VariableScope
emptyVariableScope = Map.empty
emptyVariableScopes :: VariableScopes
emptyVariableScopes = Stack.stackNew

--------------------------------------------------------------------------------

type ClassMap = Map.Map AST.ClassIdentifier (Int, Map.Map String AddressOffset)

--------------------------------------------------------------------------------

data GenState = GenState { ssm :: SSM,
                           labelSupply :: Int,
                           localAddressOffset :: AddressOffset,
                           classMap :: ClassMap }
                    deriving (Show)

type Gen a = ExceptT String (ReaderT VariableScopes (State GenState)) a

runGen :: Gen a -> (Either String a, GenState)
runGen g = runState (runReaderT (runExceptT g) emptyVariableScopes) initGenState
    where
        initGenState = GenState {ssm = [], labelSupply = 0, localAddressOffset = 0, classMap = Map.empty}

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
    return $ getVariable' scopes s
    where
        getVariable' :: VariableScopes -> String -> Maybe (Scope, AddressOffset)
        getVariable' scopes s = case Stack.stackPop scopes of
            Nothing -> Nothing
            Just (scopes', scope) -> case Map.lookup s scope of
                Nothing -> getVariable' scopes' s
                Just result -> Just (if Stack.stackIsEmpty scopes' then SGlobal else SLocal, result)

getLocalAddressOffset :: Gen Int
getLocalAddressOffset = do
    st <- get
    return $ localAddressOffset st

incrementLocalAddressOffset :: Gen ()
incrementLocalAddressOffset = do
    st <- get
    put st {localAddressOffset = localAddressOffset st + 1}

resetLocalAddressOffset :: Gen ()
resetLocalAddressOffset = do
    st <- get
    put st {localAddressOffset = 0}

getClass :: AST.ClassIdentifier -> Gen (Maybe (Int, Map.Map String AddressOffset))
getClass i = do
    st <- get
    let m = classMap st
    return $ Map.lookup i m

setClass :: AST.ClassIdentifier -> (Int, Map.Map String AddressOffset) -> Gen ()
setClass i info = do
    st <- get
    let m = classMap st
    put $ st {classMap = Map.insert i info m}

--------------------------------------------------------------------------------

gen :: AST.SPL -> Either String SSM
gen ast  = res
    where
        (res, _) = runGen $ genSPL ast

genDet :: AST.SPL -> SSM
genDet spl =
    case gen spl of
        Left err -> Trace.trace (show err) undefined
        Right ssm -> ssm

--------------------------------------------------------------------------------

genSPL :: AST.SPL -> Gen SSM
genSPL decls = do
    -- First collect class information
    collectClasses decls
    -- Process all variable declarations
    let varDecls = filter (\decl -> case decl of
            (AST.DeclV _, _) -> True
            _ -> False) decls
    let varDeclNames = map (\decl -> case decl of
            (AST.DeclV (AST.VarDeclTyped _ i _, _), _) -> Checker.idName i
            (AST.DeclV (AST.VarDeclTypedUnitialized _ i, _), _) -> Checker.idName i) varDecls
    scopes <- ask
    let scopes' = Stack.stackPush scopes (Map.fromList [(varDeclName, idx) | (idx, varDeclName) <- zip [0..] varDeclNames])
    local (const scopes') (liftM id (mapM genDecl varDecls))
    --
    genPushBackStack $ length varDecls
    -- First initialize the memory allocator
    genMallocInit
    -- Add a statement to branch to the main function
    push $ SSMLine Nothing (Just $ IControl $ CBranchSubroutine $ ALabel "main") Nothing
    -- Halt
    push $ SSMLine Nothing (Just $ IControl CHalt) Nothing
    -- Process all function declarations
    local (const scopes') (liftM id (mapM genDecl (filter (\decl -> case decl of
        (AST.DeclF _, _) -> True
        _ -> False) decls)))
    -- Generate the built-in assembly library
    genLibrary
    -- Add label to end of PC
    push $ SSMLine (Just "__end_pc") (Just $ IControl CNop) Nothing
    -- Optimize and output generated SSM
    st <- get
    return $ optimize $ ssm st

-- |Collect class information
collectClasses :: AST.SPL -> Gen ()
collectClasses [] = return ()
collectClasses ((AST.DeclC (AST.ClassDecl i vs _, _), _): ds) = do
    let size = length vs
    let strs = map Checker.declIdentifierString vs
    let assoc = zip strs [0..]
    setClass i (size, Map.fromList assoc)

collectClasses (d:ds) = collectClasses ds

genDecl :: AST.Decl -> Gen ()
genDecl (AST.DeclV varDecl, _) = genVarDecl varDecl
genDecl (AST.DeclF funDecl, _) = genFunDecl funDecl

genVarDecl :: AST.VarDecl -> Gen ()
genVarDecl (AST.VarDeclTyped _ i e, _) = genExpression e
genVarDecl (AST.VarDeclTypedUnitialized _ i, _) = return ()

genFunDecl :: AST.FunDecl -> Gen ()
genFunDecl (AST.FunDeclTyped i args _ stmts, _) = do
        resetLocalAddressOffset
        let nLocals = numLocals stmts
        -- Function entry; reserve memory space for locals
        push $ SSMLine (Just $ Checker.idName i) (Just $ IControl $ CLink $ ANumber nLocals) Nothing

        -- Calculate new scope
        let scope = [(Checker.idName arg, -offset) | (arg, offset) <- zip args [2..]]
        -- Add to old scope
        scopes <- ask
        let scopes' = Stack.stackPush scopes (Map.fromList scope)

        --local (const (0, scopes')) (liftM id (mapM genStatement stmts))
        local (const scopes') (genStatements stmts)
        push $ SSMLine Nothing (Just $ IControl CUnlink) Nothing
        push $ SSMLine Nothing (Just $ IControl CReturn) Nothing

genStatements :: [AST.Statement] -> Gen ()
genStatements [] = return ()
genStatements (stmt:stmts) = genStatement stmt stmts

genStatement :: AST.Statement -> [AST.Statement] -> Gen ()
genStatement (AST.StmtVarDecl (AST.VarDeclTyped _ i e, _), _) stmts = do
    genExpression e
    offset <- getLocalAddressOffset
    scopes <- ask
    push $ SSMLine Nothing (Just $ IStore $ SMark $ ANumber (offset + 1)) (Just $ "declare local " ++ Checker.idName i)
    case Stack.stackPop scopes of
        Just (scopes', scope) -> do
            let scope' = Map.insert (Checker.idName i) (offset + 1) scope
            incrementLocalAddressOffset
            local (const $ Stack.stackPush scopes' scope') (genStatements stmts)
genStatement (AST.StmtVarDecl (AST.VarDeclTypedUnitialized _ i, _), _) stmts = do
    offset <- getLocalAddressOffset
    scopes <- ask
    case Stack.stackPop scopes of
        Just (scopes', scope) -> do
            let scope' = Map.insert (Checker.idName i) (offset + 1) scope
            incrementLocalAddressOffset
            local (const $ Stack.stackPush scopes' scope') (genStatements stmts)
genStatement (AST.StmtIf e s, _) stmts = do
    genExpression e
    endLbl <- getFreshLabel
    push $ SSMLine Nothing (Just $ IControl $ CBranchFalse $ ALabel endLbl) Nothing
    genStatement s []
    push $ SSMLine (Just endLbl) (Just $ IControl $ CNop) Nothing
    genStatements stmts
genStatement (AST.StmtIfElse e s1 s2, _) stmts = do
    genExpression e
    elseLbl <- getFreshLabel
    endLbl <- getFreshLabel
    push $ SSMLine Nothing (Just $ IControl $ CBranchFalse $ ALabel elseLbl) Nothing
    genStatement s1 []
    push $ SSMLine Nothing (Just $ IControl $ CBranchAlways $ ALabel endLbl) Nothing
    push $ SSMLine (Just elseLbl) (Just $ IControl $ CNop) Nothing
    genStatement s2 []
    push $ SSMLine (Just endLbl) (Just $ IControl CNop) Nothing
    genStatements stmts
genStatement (AST.StmtWhile e s, _) stmts = do
    startLbl <- getFreshLabel
    endLbl <- getFreshLabel
    push $ SSMLine (Just startLbl) (Just $ IControl CNop) Nothing
    genExpression e
    push $ SSMLine Nothing (Just $ IControl $ CBranchFalse $ ALabel endLbl) Nothing
    genStatement s []
    push $ SSMLine Nothing (Just $ IControl $ CBranchAlways $ ALabel startLbl) Nothing
    push $ SSMLine (Just endLbl) (Just $ IControl CNop) Nothing
    genStatements stmts
genStatement (AST.StmtBlock stmts', _) stmts = do
    scopes <- ask
    local (const $ Stack.stackPush scopes emptyVariableScope) (genStatements stmts')
    genStatements stmts
genStatement (AST.StmtAssignment e1 e2, _) stmts = do
    genExpression e2
    genAddressOfExpression e1
    push $ SSMLine Nothing (Just $ IStore $ SAddress $ ANumber 0) Nothing
    genStatements stmts
genStatement (AST.StmtFunCall e args, p) stmts = do
    genExpression (AST.ExprFunCall e args, p)
    -- Ignore return value
    push $ SSMLine Nothing (Just $ IControl $ CAdjustSP $ ANumber $ -1) Nothing
    genStatements stmts
genStatement (AST.StmtReturn e, p) stmts = do
    genExpression e
    push $ SSMLine Nothing (Just $ IStore $ SRegister $ ARegister RReturnRegister) Nothing
    genStatement (AST.StmtReturnVoid, p) stmts
genStatement (AST.StmtReturnVoid, _) stmts = do
    push $ SSMLine Nothing (Just $ IControl CUnlink) Nothing
    push $ SSMLine Nothing (Just $ IControl CReturn) Nothing
    genStatements stmts

genExpression :: AST.Expression -> Gen ()
genExpression (AST.ExprIdentifier i, m) = do
    location <- getVariable $ Checker.idName i
    case location of
        Just (SGlobal, offset) -> do -- Load a global
            -- Load the address of the end of the program code
            push $ SSMLine Nothing (Just $ ILoad $ LConstant $ ALabel "__end_pc") (Just $ "load global " ++ Checker.idName i)
            -- Load the value at the address of (the end of the program code + offset)
            push $ SSMLine Nothing (Just $ ILoad $ LAddress $ ANumber (endPCToStartStackOffset + offset)) Nothing
        Just (SLocal, offset) -> push $ SSMLine Nothing (Just $ ILoad $ LMark $ ANumber offset) (Just $ "load local " ++ Checker.idName i)
        Nothing -> case AST.metaType m of
            Just (TFunction _ _) -> push $ SSMLine Nothing (Just $ ILoad $ LConstant $ ALabel $ Checker.idName i) (Just $ "load address of function " ++ Checker.idName i)
            _ -> throwError $ "Variable " ++ Checker.idName i ++ " not in scope"
genExpression (AST.ExprField e f, _) = do
    genExpression e
    genFields f
genExpression (AST.ExprFunCall expr@(e,m) args, _) = do
    liftM id (mapM genExpression (reverse args))
    case e of
        AST.ExprIdentifier (AST.Identifier "print", _) -> do
                    case AST.metaType m of Just (Type.TFunction [t] _) -> genPrint t
                    push $ SSMLine Nothing (Just $ ILoad $ LConstant $ ANumber $ -1) Nothing -- Load boolean True onto stack
        AST.ExprIdentifier (AST.Identifier "println", _) -> do
                    case AST.metaType m of Just (Type.TFunction [t] _) -> genPrint t
                    genPrintChar '\n'
                    push $ SSMLine Nothing (Just $ ILoad $ LConstant $ ANumber $ -1) Nothing -- Load boolean True onto stack
        AST.ExprIdentifier (AST.Identifier "isEmpty", _) -> do
                    -- Get next address of the list, if it is -1 the list is empty
                    push $ SSMLine Nothing (Just $ ILoad $ LHeap $ ANumber 0) (Just "start isEmpty")
                    push $ SSMLine Nothing (Just $ ILoad $ LConstant $ ANumber $ -1) Nothing
                    push $ SSMLine Nothing (Just $ ICompute OEq) (Just "end isEmpty")
        _ -> do
            genExpression expr
            push $ SSMLine Nothing (Just $ IControl $ CJumpSubroutine) Nothing
            -- Clean up stack
            push $ SSMLine Nothing (Just $ IControl $ CAdjustSP $ ANumber $ - length args) Nothing
            -- Load returned value
            push $ SSMLine Nothing (Just $ ILoad $ LRegister $ ARegister RReturnRegister) Nothing

    where
        genPrint :: Type.Type -> Gen ()
        genPrint Type.TBool = do
            push $ SSMLine Nothing (Just $ IControl $ CBranchFalse $ ANumber 18) (Just "start print bool")
            genPrintChar 'T'
            genPrintChar 'r'
            genPrintChar 'u'
            genPrintChar 'e'
            push $ SSMLine Nothing (Just $ IControl $ CBranchAlways $ ANumber 20) Nothing
            genPrintChar 'F'
            genPrintChar 'a'
            genPrintChar 'l'
            genPrintChar 's'
            genPrintChar 'e'
        genPrint Type.TInt = push $ SSMLine Nothing (Just $ IIO IOPrintInt) Nothing
        genPrint Type.TChar = push $ SSMLine Nothing (Just $ IIO IOPrintChar) Nothing
        genPrint (Type.TTuple t1 t2) = do
            genPrintChar '('
            push $ SSMLine Nothing (Just $ ILoad $ LHeapMultiple (ANumber 0) (ANumber 2)) Nothing
            genPrint t1
            genPrintChar ','
            genPrint t2
            genPrintChar ')'
        genPrint (Type.TList a) = do
            lblStart <- getFreshLabel
            lblCleanUp <- getFreshLabel
            genPrintChar '['
            -- Copy heap address
            push $ SSMLine (Just lblStart) (Just $ IStore $ SStack $ ANumber $ 1) Nothing
            push $ SSMLine Nothing (Just $ IControl $ CAdjustSP $ ANumber $ 2) Nothing
            -- Get next address of the list, if it is -1 the list is empty
            push $ SSMLine Nothing (Just $ ILoad $ LHeap $ ANumber $ 0) Nothing
            push $ SSMLine Nothing (Just $ ILoad $ LConstant $ ANumber $ -1) Nothing
            push $ SSMLine Nothing (Just $ ICompute OEq) Nothing
            -- If the list is empty, skip loop -- jump to clean up
            push $ SSMLine Nothing (Just $ IControl $ CBranchTrue $ ALabel lblCleanUp) Nothing
            -- Otherwise load the value of the list, first copy heap address again
            push $ SSMLine Nothing (Just $ IStore $ SStack $ ANumber $ 1) Nothing
            push $ SSMLine Nothing (Just $ IControl $ CAdjustSP $ ANumber $ 2) Nothing
            -- Load value
            push $ SSMLine Nothing (Just $ ILoad $ LHeap $ ANumber $ -1) Nothing
            -- Recursively print
            genPrint a
            genPrintChar ','
            -- Load the next address of the list
            push $ SSMLine Nothing (Just $ ILoad $ LHeap $ ANumber $ 0) Nothing
            -- Jump back to start of loop
            push $ SSMLine Nothing (Just $ IControl $ CBranchAlways $ ALabel lblStart) Nothing
            -- Clean up
            push $ SSMLine (Just lblCleanUp) (Just $ IControl $ CAdjustSP $ ANumber $ -1) Nothing
            genPrintChar ']'
        genPrint (Type.TPointer _) = genFunCall "printHex" 1
        genPrint (Type.TVar _) = return ()

genExpression (AST.ExprConstant c, _) = genConstant c
genExpression (AST.ExprTuple e1 e2, _) = do
    genExpression e2
    genExpression e1
    push $ SSMLine Nothing (Just $ IStore $ SHeapMultiple $ ANumber 2) (Just "tuple")
genExpression (AST.ExprUnaryOp op e, _) = do
    case op of
        (AST.UnaryOpReference, _) -> genAddressOfExpression e
        _ -> genExpression e >> genUnaryOp op
genExpression (AST.ExprBinaryOp op e1@(_, m1) e2@(_, m2), _) = do
    case AST.metaType m1 of
        Just t1 ->
            case AST.metaType m2 of
                Just t2 ->  do
                    genExpression e1
                    genExpression e2
                    genBinaryOp op t1 t2
        Nothing -> throwError $ show m1 ++ show m2
genExpression (AST.ExprNew i, _) = do
    Just (size, _) <- getClass i
    push $ SSMLine Nothing (Just $ ILoad $ LConstant $ ANumber size) (Just $ "allocate size for " ++ show i)
    genFunCall "malloc" 1
     -- Load return register twice, once for the fun call to init, and once as
     -- the return value of the new operator
    push $ SSMLine Nothing (Just $ ILoad $ LRegister $ ARegister RReturnRegister) Nothing
    push $ SSMLine Nothing (Just $ ILoad $ LRegister $ ARegister RReturnRegister) Nothing
    genFunCall (Checker.classIDName i ++ ".__init__") 1

genFields :: [AST.Field] -> Gen ()
genFields fields = liftM id (mapM genField fields) >> return ()
    where
        genField :: AST.Field -> Gen ()
        genField (AST.FieldHd, _) = push $ SSMLine Nothing (Just $ ILoad $ LHeap $ ANumber $ -1) Nothing
        genField (AST.FieldTl, _) = push $ SSMLine Nothing (Just $ ILoad $ LHeap $ ANumber 0) Nothing
        genField (AST.FieldFst, _) = push $ SSMLine Nothing (Just $ ILoad $ LHeap $ ANumber 0) Nothing
        genField (AST.FieldSnd, _) = push $ SSMLine Nothing (Just $ ILoad $ LHeap $ ANumber $ -1) Nothing

genAddressOfExpression :: AST.Expression -> Gen ()
genAddressOfExpression (AST.ExprIdentifier i, m) = do
    location <- getVariable $ Checker.idName i
    case location of
        Just (SGlobal, offset) -> do -- Load a global
            -- Load the address of the end of the program code
            push $ SSMLine Nothing (Just $ ILoad $ LConstant $ ALabel "__end_pc") (Just $ "load address of global " ++ Checker.idName i)
            -- Load the value at the address of (the end of the program code + offset)
            push $ SSMLine Nothing (Just $ ILoad $ LConstant $ ANumber (endPCToStartStackOffset + offset)) Nothing
            push $ SSMLine Nothing (Just $ ICompute OAdd) Nothing
        Just (SLocal, offset) -> push $ SSMLine Nothing (Just $ ILoad $ LLocalAddress $ ANumber offset) (Just $ "load address of local " ++ Checker.idName i)
        Nothing -> throwError $ "Variable " ++ Checker.idName i ++ " not in scope"
genAddressOfExpression (AST.ExprField e f, _) = do
    genExpression e
    genAddressOfFields f
genAddressOfExpression (AST.ExprUnaryOp (AST.UnaryOpDereference, _) e, _) = genExpression e
genAddressOfExpression _ = throwError "cannot take address of a temporary value expression"

genAddressOfFields :: [AST.Field] -> Gen ()
genAddressOfFields fields = do
    genFields $ init fields
    case last fields of
        (AST.FieldHd, _) -> do
            push $ SSMLine Nothing (Just $ ILoad $ LConstant $ ANumber $ -1) Nothing
            push $ SSMLine Nothing (Just $ ICompute OAdd) Nothing
        (AST.FieldSnd, _) -> do
            push $ SSMLine Nothing (Just $ ILoad $ LConstant $ ANumber $ -1) Nothing
            push $ SSMLine Nothing (Just $ ICompute OAdd) Nothing
        _ -> return ()

genConstant :: AST.Constant -> Gen ()
genConstant (AST.ConstInt i, _) = push $ SSMLine Nothing (Just $ ILoad $ LConstant $ ANumber i) Nothing
genConstant (AST.ConstChar c, _) = push $ SSMLine Nothing (Just $ ILoad $ LConstant $ ANumber $ Char.ord c) Nothing
genConstant (AST.ConstBool b, _) = let n = if b then -1 else 0 in
    push $ SSMLine Nothing (Just $ ILoad $ LConstant $ ANumber $ n) Nothing
genConstant (AST.ConstEmptyList, _) = do
        push $ SSMLine Nothing (Just $ ILoad $ LConstant $ ANumber $ -1) Nothing
        push $ SSMLine Nothing (Just $ IStore SHeap) Nothing
        -- Adjust SP to point back
        push $ SSMLine Nothing (Just $ IControl $ CAdjustSP $ ANumber $ -1) Nothing
        push $ SSMLine Nothing (Just $ ILoad $ LConstant $ ANumber $ -1) Nothing
        push $ SSMLine Nothing (Just $ IStore SHeap) Nothing

genUnaryOp :: AST.UnaryOperator -> Gen ()
genUnaryOp (AST.UnaryOpNeg, _) = push $ SSMLine Nothing (Just $ ICompute ONot) Nothing
genUnaryOp (AST.UnaryOpBitwiseNot, _) = push $ SSMLine Nothing (Just $ ICompute ONot) Nothing
genUnaryOp (AST.UnaryOpSubtr, _) = push $ SSMLine Nothing (Just $ ICompute ONeg) Nothing
genUnaryOp (AST.UnaryOpCast _, _) = return ()
genUnaryOp (AST.UnaryOpDereference, _) = push $ SSMLine Nothing (Just $ ILoad $ LAddress $ ANumber 0) Nothing

genBinaryOp :: AST.BinaryOperator -> Type.Type -> Type.Type -> Gen ()
genBinaryOp (AST.BinaryOpOr, _) _ _ = push $ SSMLine Nothing (Just $ ICompute OOr) Nothing
genBinaryOp (AST.BinaryOpBitwiseOr, _) _ _ = push $ SSMLine Nothing (Just $ ICompute OOr) Nothing
genBinaryOp (AST.BinaryOpAnd, _) _ _ = push $ SSMLine Nothing (Just $ ICompute OAnd) Nothing
genBinaryOp (AST.BinaryOpBitwiseAnd, _) _ _ = push $ SSMLine Nothing (Just $ ICompute OAnd) Nothing
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
genBinaryOp (AST.BinaryOpConcat, _) _ _ = push $ SSMLine Nothing (Just $ IStore $ SHeapMultiple $ ANumber 2) (Just "concat")
genBinaryOp (AST.BinaryOpPlus, _) _ _ = push $ SSMLine Nothing (Just $ ICompute OAdd) Nothing
genBinaryOp (AST.BinaryOpSubtr, _) _ _ = push $ SSMLine Nothing (Just $ ICompute OSub) Nothing
genBinaryOp (AST.BinaryOpReferencePlus, _) _ _ = push $ SSMLine Nothing (Just $ ICompute OAdd) Nothing
genBinaryOp (AST.BinaryOpReferenceSubtr, _) _ _ = push $ SSMLine Nothing (Just $ ICompute OSub) Nothing
genBinaryOp (AST.BinaryOpReferenceReferenceSubtr, _) _ _ = push $ SSMLine Nothing (Just $ ICompute OSub) Nothing
genBinaryOp (AST.BinaryOpMult, _) _ _ = push $ SSMLine Nothing (Just $ ICompute OMul) Nothing
genBinaryOp (AST.BinaryOpDiv, _) _ _ = push $ SSMLine Nothing (Just $ ICompute ODiv) Nothing
genBinaryOp (AST.BinaryOpMod, _) _ _ = push $ SSMLine Nothing (Just $ ICompute OMod) Nothing
genBinaryOp (AST.BinaryOpBitShiftLeft, _) _ _ = push $ SSMLine Nothing (Just $ ICompute OLsl) Nothing
genBinaryOp (AST.BinaryOpBitShiftRight, _) _ _ = push $ SSMLine Nothing (Just $ ICompute OLsr) Nothing

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

genFunCall :: String -> Int -> Gen ()
genFunCall str numArgs = do
    -- Call function
    push $ SSMLine Nothing (Just $ IControl $ CBranchSubroutine $ ALabel str) Nothing
    -- Clean up stack
    push $ SSMLine Nothing (Just $ IControl $ CAdjustSP $ ANumber $ - numArgs) Nothing
    -- Load returned value
    push $ SSMLine Nothing (Just $ ILoad $ LRegister $ ARegister RReturnRegister) Nothing

--------------------------------------------------------------------------------

class Locals a where
    numLocals :: a -> Int

instance Locals AST.FunDecl where
    numLocals (AST.FunDeclTyped _ args _ stmts, _) = numLocals stmts

instance Locals [AST.Statement] where
    numLocals stmts = sum (map numLocals stmts)

instance Locals AST.Statement where
    numLocals (AST.StmtVarDecl _, _) = 1
    numLocals (AST.StmtIf _ s, _) = numLocals s
    numLocals (AST.StmtIfElse _ s1 s2, _) = numLocals s1 + numLocals s2
    numLocals (AST.StmtBlock stmts, _) = numLocals stmts
    numLocals (AST.StmtWhile _ s, _) = numLocals s
    numLocals _ = 0

--------------------------------------------------------------------------------
-- Built-in assembly library functions

-- Push back the start of the stack
genPushBackStack :: Int -> Gen ()
genPushBackStack n = do
    push $ SSMLine Nothing (Just $ ILoad $ LRegister $ ARegister RStackPointer) (Just $ "push back stack to make room for " ++ show n ++ " globals")
    push $ SSMLine Nothing (Just $ ILoad $ LConstant $ ANumber n) Nothing
    push $ SSMLine Nothing (Just $ ICompute OAdd) Nothing
    push $ SSMLine Nothing (Just $ IStore $ SRegister $ ARegister RStackPointer) Nothing

genLibrary :: Gen ()
genLibrary = do
    genLength
    genMalloc

genLength :: Gen ()
genLength = do
    start <- getFreshLabel
    end <- getFreshLabel
    push $ SSMLine (Just "length") (Just $ IControl $ CLink $ ANumber 2) Nothing
    -- Start with 0 length
    push $ SSMLine Nothing (Just $ ILoad $ LConstant $ ANumber 0) (Just "start length")
    push $ SSMLine Nothing (Just $ IStore $ SMark $ ANumber 1) Nothing
    -- Copy heap address
    push $ SSMLine Nothing (Just $ ILoad $ LMark $ ANumber $ -2) Nothing
    push $ SSMLine Nothing (Just $ IStore $ SMark $ ANumber 2) Nothing
    -- Get next address of the list, if it is -1 the list is empty
    push $ SSMLine (Just start) (Just $ ILoad $ LMark $ ANumber 2) Nothing
    push $ SSMLine Nothing (Just $ ILoad $ LConstant $ ANumber $ -1) Nothing
    push $ SSMLine Nothing (Just $ ICompute OEq) Nothing
    -- If the list is empty, skip loop -- jump to clean up
    push $ SSMLine Nothing (Just $ IControl $ CBranchTrue $ ALabel end) Nothing
    -- Otherwise load the next address of the list
    push $ SSMLine Nothing (Just $ ILoad $ LMark $ ANumber 2) Nothing
    push $ SSMLine Nothing (Just $ ILoad $ LHeap $ ANumber 0) Nothing
    push $ SSMLine Nothing (Just $ IStore $ SMark $ ANumber 2) Nothing
    -- And increment the counter
    push $ SSMLine Nothing (Just $ ILoad $ LMark $ ANumber 1) Nothing
    genUtilIncrement
    push $ SSMLine Nothing (Just $ IStore $ SMark $ ANumber 1) Nothing
    push $ SSMLine Nothing (Just $ IControl $ CBranchAlways $ ALabel start) Nothing
    -- Clean up
    push $ SSMLine (Just end) (Just $ ILoad $ LMark $ ANumber 1) Nothing
    push $ SSMLine Nothing (Just $ IStore $ SRegister $ ARegister RReturnRegister) Nothing
    push $ SSMLine Nothing (Just $ IControl CUnlink) Nothing
    push $ SSMLine Nothing (Just $ IControl CReturn) Nothing

mAllocSmallestBlockSize :: Int
--mAllocSmallestBlockSize = 1024     -- 2^10
mAllocSmallestBlockSize = 524288
mAllocLargestBlockSize :: Int
mAllocLargestBlockSize = 1048576 -- 2^20
mAllocNumBlocks :: Int
mAllocNumBlocks = ceiling $ (fromIntegral mAllocLargestBlockSize) / (fromIntegral mAllocSmallestBlockSize)

genMallocInit :: Gen ()
genMallocInit = do
    push $ SSMLine Nothing (Just $ ILoad $ LConstant $ ANumber mAllocNumBlocks) Nothing
    push $ SSMLine Nothing (Just $ ILoad $ LConstant $ ANumber mAllocSmallestBlockSize) Nothing
    push $ SSMLine Nothing (Just $ ILoad $ LRegister $ ARegister RHeapPointer) Nothing
    genFunCall "__malloc_init" 3

genMalloc :: Gen ()
genMalloc = do
    push $ SSMLine (Just "malloc") (Just $ IControl $ CLink $ ANumber 0) Nothing
    push $ SSMLine Nothing (Just $ ILoad $ LMark $ ANumber $ -2) Nothing
    genFunCall "__malloc" 1 -- return register value set by __malloc will be used
    push $ SSMLine Nothing (Just $ IControl CUnlink) Nothing
    push $ SSMLine Nothing (Just $ IControl CReturn) Nothing


--------------------------------------------------------------------------------

optimize :: SSM -> SSM
optimize = removeNops []
    where
        removeNops :: SSM -> SSM -> SSM
        removeNops accum [] = accum
        removeNops accum [ssm] = removeNops (accum ++ [ssm]) []
        removeNops accum (ssm1:ssm2:ssms) =
            case ssm1 of
                SSMLine (Just lbl) (Just (IControl CNop)) _ ->
                    case ssm2 of
                        SSMLine Nothing i c -> removeNops accum (SSMLine (Just lbl) i c : ssms)
                        SSMLine (Just lbl') i c -> removeNops (renameLabel lbl lbl' accum) (renameLabel lbl lbl' (ssm2 : ssms))
                _ -> removeNops (accum ++ [ssm1]) (ssm2 : ssms)

class SSMPost a where
    renameLabel :: String -> String -> a -> a
    renameLabel _ _ a = a

instance SSMPost a => SSMPost (Maybe a) where
    renameLabel _ _ Nothing = Nothing
    renameLabel f t (Just x) = Just $ renameLabel f t x

instance SSMPost SSM where
    renameLabel f t = map (renameLabel f t)

instance SSMPost SSMLine where
    renameLabel f t (SSMLine l o c) = SSMLine (renameLabel f t l) (renameLabel f t o) (renameLabel f t c)

instance SSMPost SSMInstruction where
    renameLabel f t (ILoad l) = ILoad (renameLabel f t l)
    renameLabel f t (IStore l) = IStore (renameLabel f t l)
    renameLabel f t (IControl l) = IControl (renameLabel f t l)
    renameLabel f t (IAnnotate a1 a2 a3 a4 a5) = IAnnotate (renameLabel f t a1) (renameLabel f t a2) (renameLabel f t a3) (renameLabel f t a4) (renameLabel f t a5)
    renameLabel _ _ i = i

instance SSMPost SSMStore where
    renameLabel f t (SStack a) = SStack $ renameLabel f t a
    renameLabel f t SHeap = SHeap
    renameLabel f t (SHeapMultiple a) = SHeapMultiple $ renameLabel f t a
    renameLabel f t (SMark a) = SMark $ renameLabel f t a
    renameLabel f t (SAddress a) = SAddress $ renameLabel f t a
    renameLabel f t (SRegister a) = SRegister $ renameLabel f t a

instance SSMPost SSMLoad where
    renameLabel f t (LConstant a) = LConstant $ renameLabel f t a
    renameLabel f t (LStack a) = LStack $ renameLabel f t a
    renameLabel f t (LHeap a) = LHeap $ renameLabel f t a
    renameLabel f t (LHeapMultiple a1 a2) = LHeapMultiple (renameLabel f t a1) (renameLabel f t a2)
    renameLabel f t (LMark a) = LMark $ renameLabel f t a
    renameLabel f t (LAddress a) = LAddress $ renameLabel f t a
    renameLabel f t (LLocalAddress a) = LLocalAddress $ renameLabel f t a
    renameLabel f t (LRegister a) = LRegister $ renameLabel f t a
    renameLabel f t (LRegisterFromRegister a1 a2) = LRegisterFromRegister (renameLabel f t a1) (renameLabel f t a2)
    renameLabel f t (LStackAddress a) = LStackAddress $ renameLabel f t a
    renameLabel f t (LMarkAddress a) = LMarkAddress $ renameLabel f t a
    renameLabel f t (LAddressAddress a) = LAddressAddress $ renameLabel f t a

instance SSMPost SSMControl where
    renameLabel f t (CBranchEq a) = CBranchEq $ renameLabel f t a
    renameLabel f t (CBranchNeq a) = CBranchNeq $ renameLabel f t a
    renameLabel f t (CBranchLt a) = CBranchLt $ renameLabel f t a
    renameLabel f t (CBranchGt a) = CBranchGt $ renameLabel f t a
    renameLabel f t (CBranchLe a) = CBranchLe $ renameLabel f t a
    renameLabel f t (CBranchGe a) = CBranchGe $ renameLabel f t a
    renameLabel f t (CBranchAlways a) = CBranchAlways $ renameLabel f t a
    renameLabel f t (CBranchFalse a) = CBranchFalse $ renameLabel f t a
    renameLabel f t (CBranchTrue a) = CBranchTrue $ renameLabel f t a
    renameLabel f t (CBranchSubroutine a) = CBranchSubroutine $ renameLabel f t a
    renameLabel f t (CLink a) = CLink $ renameLabel f t a
    renameLabel f t (CAdjustSP a) = CAdjustSP $ renameLabel f t a
    renameLabel _ _ c = c

instance SSMPost SSMArgument where
    renameLabel f t (ALabel lbl) = ALabel $ renameLabel f t lbl
    renameLabel _ _ a = a

instance SSMPost SSMLabel where
    renameLabel f t lbl = if f == lbl then t else lbl
