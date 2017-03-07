module Data.AST where

import Data.Pos

type SPL             = [Decl]
data Decl'           = DeclV VarDecl
                     | DeclF FunDecl
                       deriving (Show)
type Decl            = (Decl', Pos)

data VarDecl'        = VarDeclTyped Type Identifier Expression
                     | VarDeclUntyped Identifier Expression
                       deriving (Show)
type VarDecl         = (VarDecl', Pos)

data FunDecl'        = FunDeclTyped Identifier [Identifier] FunType [VarDecl] [Statement] -- [Identifier] are the arguments
                     | FunDeclUntyped Identifier [Identifier] [VarDecl] [Statement]
                       deriving (Show)
type FunDecl         = (FunDecl', Pos)

data FunType'        = FunType [Type] Type
                     | FunTypeVoid [Type]
                       deriving (Show)
type FunType         = (FunType', Pos)

data Type'           = TypeTuple Type Type
                     | TypeList Type
                     | TypeInt
                     | TypeBool
                     | TypeChar
                     | TypeIdentifier Identifier
                       deriving (Show)
type Type            = (Type', Pos)

data Statement'      = StmtIf Expression [Statement]
                     | StmtIfElse Expression [Statement] [Statement]
                     | StmtWhile Expression [Statement]
                     | StmtAssignment Identifier Expression
                     | StmtAssignmentField Identifier [Field] Expression
                     | StmtFunCall Identifier [Expression]
                     | StmtReturn Expression
                     | StmtReturnVoid
                       deriving (Show)
type Statement       = (Statement', Pos)

data Expression'     = ExprIdentifier Identifier
                     | ExprIdentifierField Identifier [Field]
                     | ExprFunCall Identifier [Expression]
                     | ExprConstant Constant
                     | ExprTuple Expression Expression
                     | ExprUnaryOp UnaryOperator Expression
                     | ExprBinaryOp BinaryOperator Expression Expression
                       deriving (Show)
type Expression      = (Expression', Pos)

data Constant'       = ConstBool Bool
                     | ConstInt Int
                     | ConstChar Char
                     | ConstEmptyList
                       deriving (Show)
type Constant        = (Constant', Pos)

data UnaryOperator'  = UnaryOpNeg
                     | UnaryOpSubtr
                       deriving (Show)
type UnaryOperator   = (UnaryOperator', Pos)

data BinaryOperator' = BinaryOpOr
                     | BinaryOpAnd
                     | BinaryOpEq
                     | BinaryOpNEq
                     | BinaryOpLT
                     | BinaryOpGT
                     | BinaryOpLTE
                     | BinaryOpGTE
                     | BinaryOpConcat
                     | BinaryOpPlus
                     | BinaryOpSubtr
                     | BinaryOpMult
                     | BinaryOpDiv
                     | BinaryOpMod
                       deriving (Show)
type BinaryOperator  = (BinaryOperator', Pos)

data Field'          = FieldHd
                     | FieldTl
                     | FieldFst
                     | FieldSnd
                       deriving (Show)
type Field           = (Field', Pos)

newtype Identifier'  = Identifier String
                       deriving (Show)
type Identifier      = (Identifier', Pos)

binaryOperatorPrecedence' :: BinaryOperator' -> Int
binaryOperatorPrecedence' BinaryOpOr = 1
binaryOperatorPrecedence' BinaryOpAnd = 2
binaryOperatorPrecedence' BinaryOpEq = 3
binaryOperatorPrecedence' BinaryOpNEq = 3
binaryOperatorPrecedence' BinaryOpLT = 4
binaryOperatorPrecedence' BinaryOpGT = 4
binaryOperatorPrecedence' BinaryOpLTE = 4
binaryOperatorPrecedence' BinaryOpGTE = 4
binaryOperatorPrecedence' BinaryOpConcat = 5
binaryOperatorPrecedence' BinaryOpPlus = 6
binaryOperatorPrecedence' BinaryOpSubtr = 6
binaryOperatorPrecedence' BinaryOpMult = 7
binaryOperatorPrecedence' BinaryOpDiv = 7
binaryOperatorPrecedence' BinaryOpMod = 7

binaryOperatorPrecedence :: BinaryOperator -> Int
binaryOperatorPrecedence (bOp, _) = binaryOperatorPrecedence' bOp
