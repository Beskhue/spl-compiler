{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.AST where

import Data.Pos

class PrettyPrint a where
    prettyPrint :: a -> String


type SPL             = [Decl]

{-
instance PrettyPrint SPL where
    prettyPrint (decl:decls) = prettyPrint decl ++ "\n"
    prettyPrint [] = ""
-}

instance (PrettyPrint a) => PrettyPrint [a] where
    prettyPrint ss = printMultiple 0 ss
        where
            printMultiple _ [] = ""
            printMultiple 0 (s:ss) = prettyPrint s ++ printMultiple 1 ss
            printMultiple n (s:ss) = "\n" ++ prettyPrint s ++ printMultiple (n+1) ss

data Decl'           = DeclV VarDecl
                     | DeclF FunDecl
                       deriving (Show)
type Decl            = (Decl', Pos)

instance PrettyPrint Decl where
    prettyPrint (DeclV v, _) = prettyPrint v
    prettyPrint (DeclF f, _) = prettyPrint f

data VarDecl'        = VarDeclTyped Type Identifier Expression
                     | VarDeclUntyped Identifier Expression
                       deriving (Show)
type VarDecl         = (VarDecl', Pos)

instance PrettyPrint VarDecl where
    prettyPrint (VarDeclTyped t i e, _) = prettyPrint t ++ " " ++ prettyPrint i ++ " = " ++ prettyPrint e ++ ";"
    prettyPrint (VarDeclUntyped i e, _) = "Var " ++ prettyPrint i ++ " = " ++ prettyPrint e ++ ";"

data FunDecl'        = FunDeclTyped Identifier [Identifier] FunType [VarDecl] [Statement] -- [Identifier] are the arguments
                     | FunDeclUntyped Identifier [Identifier] [VarDecl] [Statement]
                       deriving (Show)
type FunDecl         = (FunDecl', Pos)

instance PrettyPrint FunDecl where
    prettyPrint (f, _) = case f of
        FunDeclTyped name args fType v s ->
            prettyPrint name
            ++ " (" ++ printArgs 0 args ++ ") :: " ++ prettyPrint fType ++ " {\n"
            ++ indent (prettyPrint v) ++ (if length v > 0 then "\n\n" else "")
            ++ indent (prettyPrint s) ++ "\n"
            ++ "}"
        FunDeclUntyped name args v s ->
            prettyPrint name
            ++ " (" ++ printArgs 0 args ++ ") {\n"
            ++ indent (prettyPrint v) ++ (if length v > 0 then "\n\n" else "")
            ++ indent (prettyPrint s) ++ "\n"
            ++ "}"
        where
            printArgs :: Int -> [Identifier] -> String
            printArgs _ [] = ""
            printArgs 0 (arg:args) = prettyPrint arg ++ printArgs 1 args
            printArgs n (arg:args) = ", " ++ prettyPrint arg ++ printArgs 2 args

data FunType'        = FunType [Type] Type
                     | FunTypeVoid [Type]
                       deriving (Show)
type FunType         = (FunType', Pos)

instance PrettyPrint FunType where
    prettyPrint (f, _) = case f of
        FunType argTypes returnType -> printArgsTypes argTypes ++ "-> " ++ prettyPrint returnType
        FunTypeVoid argTypes -> printArgsTypes argTypes ++ "-> Void"
        where
            printArgsTypes :: [Type] -> String
            printArgsTypes [] = ""
            printArgsTypes (arg:args) = prettyPrint arg ++ " " ++ printArgsTypes args

data Type'           = TypeTuple Type Type
                     | TypeList Type
                     | TypeInt
                     | TypeBool
                     | TypeChar
                     | TypeIdentifier Identifier
                       deriving (Show)
type Type            = (Type', Pos)

instance PrettyPrint Type where
    prettyPrint (TypeTuple t1 t2, _) = "(" ++ prettyPrint t1 ++ "," ++ prettyPrint t2 ++ ")"
    prettyPrint (TypeList t, _) = "[" ++ prettyPrint t ++ "]"
    prettyPrint (TypeInt, _) = "Int"
    prettyPrint (TypeBool, _) = "Bool"
    prettyPrint (TypeChar, _) = "Char"
    prettyPrint (TypeIdentifier i, _) = prettyPrint i

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

instance PrettyPrint Statement where
    prettyPrint (StmtIf e ss, _) = "if ("
        ++ prettyPrint e
        ++ ") {"
        ++ indent (prettyPrint ss)
        ++ "}"
    prettyPrint (StmtIfElse e ss1 ss2, _) = "if ("
        ++ prettyPrint e
        ++ ") {"
        ++ indent (prettyPrint ss1)
        ++ "} else {"
        ++ indent (prettyPrint ss2)
        ++ "}"
    prettyPrint (StmtWhile e ss, _) = "while ("
        ++ prettyPrint e
        ++ ") {"
        ++ indent (prettyPrint ss)
        ++ "}"
    prettyPrint (StmtAssignment i e, _) = prettyPrint i ++ " = " ++ prettyPrint e ++ ";"
    prettyPrint (StmtFunCall i es, _) = prettyPrint i ++ "(" ++ printArgs 0 es ++ ");"
        where
            printArgs :: Int -> [Expression] -> String
            printArgs _ [] = ""
            printArgs 0 (e:es) = prettyPrint e ++ printArgs 1 es
            printArgs n (e:es) = ", " ++ prettyPrint e ++ printArgs (n+1) es
    prettyPrint (StmtReturn e, _) = "return " ++ prettyPrint e ++ ";"
    prettyPrint (StmtReturnVoid, _) = "return;"

data Expression'     = ExprIdentifier Identifier
                     | ExprIdentifierField Identifier [Field]
                     | ExprFunCall Identifier [Expression]
                     | ExprConstant Constant
                     | ExprTuple Expression Expression
                     | ExprUnaryOp UnaryOperator Expression
                     | ExprBinaryOp BinaryOperator Expression Expression
                       deriving (Show)
type Expression      = (Expression', Pos)

instance PrettyPrint Expression where
    prettyPrint (ExprIdentifier i, _) = prettyPrint i
    prettyPrint (ExprIdentifierField i field, _) = prettyPrint i ++ printField field
        where
            printField :: [Field] -> String
            printField = foldr ((++) . prettyPrint) ""
    prettyPrint (ExprFunCall i es, _) = prettyPrint i ++ "(" ++ printArgs 0 es ++ ")"
        where
            printArgs :: Int -> [Expression] -> String
            printArgs _ [] = ""
            printArgs 0 (e:es) = prettyPrint e ++ printArgs 1 es
            printArgs n (e:es) = ", " ++ prettyPrint e ++ printArgs (n+1) es
    prettyPrint (ExprConstant c, _) = prettyPrint c
    prettyPrint (ExprTuple e1 e2, _) = "(" ++ prettyPrint e1 ++ ", " ++ prettyPrint e2 ++ ")"
    prettyPrint (ExprUnaryOp op e, _) = prettyPrint op ++ prettyPrint e
    prettyPrint (ExprBinaryOp op e1 e2, _) = print' e1 ++ prettyPrint op ++ print' e2
        where
            print' :: Expression -> String
            print' e = case e of
                (ExprBinaryOp op' _ _, _) -> if binaryOperatorPrecedence op' < binaryOperatorPrecedence op
                    then "(" ++ prettyPrint e ++ ")"
                    else prettyPrint e
                _ -> prettyPrint e

data Constant'       = ConstBool Bool
                     | ConstInt Int
                     | ConstChar Char
                     | ConstEmptyList
                       deriving (Show)
type Constant        = (Constant', Pos)

instance PrettyPrint Constant where
    prettyPrint (ConstBool b, _) = show b
    prettyPrint (ConstInt i, _) = show i
    prettyPrint (ConstChar c, _) = show c
    prettyPrint (ConstEmptyList, _) = "[]"

data UnaryOperator'  = UnaryOpNeg
                     | UnaryOpSubtr
                       deriving (Show)
type UnaryOperator   = (UnaryOperator', Pos)

instance PrettyPrint UnaryOperator where
    prettyPrint (UnaryOpNeg, _) = "!"
    prettyPrint (UnaryOpSubtr, _) = "-"

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

instance PrettyPrint BinaryOperator where
    prettyPrint (BinaryOpOr, _) = "||"
    prettyPrint (BinaryOpAnd, _) = "&&"
    prettyPrint (BinaryOpEq, _) = "=="
    prettyPrint (BinaryOpNEq, _) = "!="
    prettyPrint (BinaryOpLT, _) = "<"
    prettyPrint (BinaryOpGT, _) = ">"
    prettyPrint (BinaryOpLTE, _) = "<="
    prettyPrint (BinaryOpGTE, _) = ">="
    prettyPrint (BinaryOpConcat, _) = ":"
    prettyPrint (BinaryOpPlus, _) = "+"
    prettyPrint (BinaryOpSubtr, _) = "-"
    prettyPrint (BinaryOpMult, _) = "*"
    prettyPrint (BinaryOpDiv, _) = "/"
    prettyPrint (BinaryOpMod, _) = "%"

data Field'          = FieldHd
                     | FieldTl
                     | FieldFst
                     | FieldSnd
                       deriving (Show)
type Field           = (Field', Pos)

instance PrettyPrint Field where
    prettyPrint (FieldHd, _) = ".hd"
    prettyPrint (FieldTl, _) = ".tl"
    prettyPrint (FieldFst, _) = ".fst"
    prettyPrint (FieldSnd, _) = ".snd"

newtype Identifier'  = Identifier String
                       deriving (Show)
type Identifier      = (Identifier', Pos)

instance PrettyPrint Identifier where
    prettyPrint (Identifier i, _) = i

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

data Associativity = ALeft | ARight

binaryOperatorAssociativity' :: BinaryOperator' -> Associativity
binaryOperatorAssociativity' BinaryOpOr = ALeft
binaryOperatorAssociativity' BinaryOpAnd = ALeft
binaryOperatorAssociativity' BinaryOpEq = ALeft
binaryOperatorAssociativity' BinaryOpNEq = ALeft
binaryOperatorAssociativity' BinaryOpLT = ALeft
binaryOperatorAssociativity' BinaryOpGT = ALeft
binaryOperatorAssociativity' BinaryOpLTE = ALeft
binaryOperatorAssociativity' BinaryOpGTE = ALeft
binaryOperatorAssociativity' BinaryOpConcat = ARight
binaryOperatorAssociativity' BinaryOpPlus = ALeft
binaryOperatorAssociativity' BinaryOpSubtr = ALeft
binaryOperatorAssociativity' BinaryOpMult = ALeft
binaryOperatorAssociativity' BinaryOpDiv = ALeft
binaryOperatorAssociativity' BinaryOpMod = ALeft

binaryOperatorAssociativity :: BinaryOperator -> Associativity
binaryOperatorAssociativity (bOp, _) = binaryOperatorAssociativity' bOp

indent :: String -> String
indent ""  = ""
indent str = "    " ++ indent' str
    where
        indent' :: String -> String
        indent' ""        = ""
        indent' ('\n':cs) = "\n    " ++ indent' cs
        indent' (c:cs)    = c : indent' cs
