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
    display (CBranchSubroutine arg) = "brs " ++ display arg
    display CJumpSubroutine = "jsr"
    display CReturn = "ret"
    display (CLink arg) = "link " ++ display arg
    display CUnlink = "unlink"
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

newtype GenState = GenState { astAnnotation :: Checker.ASTAnnotation }
                    deriving (Show)

type Gen a = ExceptT String (State GenState) a

runGen :: Gen a -> Checker.ASTAnnotation -> (Either String a, GenState)
runGen g astAnnotation = runState (runExceptT g) initGenState
    where
        initGenState = GenState {astAnnotation = astAnnotation}

--------------------------------------------------------------------------------

gen :: AST.SPL -> Checker.ASTAnnotation -> Either String SSM
gen ast annotation = res
    where
        (res, _) = runGen (genSPL ast) annotation

genDet :: AST.SPL -> Checker.ASTAnnotation -> SSM
genDet spl annotation =
    case gen spl annotation of
        Left err -> Trace.trace (show err) undefined
        Right ssm -> ssm

--------------------------------------------------------------------------------

genSPL :: AST.SPL -> Gen SSM
genSPL = undefined
