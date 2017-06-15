{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.AST where

import Data.Pos
import qualified Data.Type as Type

class PrettyPrint a where
    prettyPrint :: a -> String

class ASTEq a where
    astEq :: a -> a -> Bool

type SPL             = [Decl]

data Meta = Meta {metaPos :: Pos, metaType :: Maybe Type.Type}
            deriving (Ord, Eq, Show)

metaFromPos :: Pos -> Meta
metaFromPos p = Meta {metaPos = p, metaType = Nothing}

emptyMeta :: Meta
emptyMeta = Meta {metaPos = emptyPos, metaType = Nothing}

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

instance (ASTEq a) => ASTEq [a] where
    astEq [] [] = True
    astEq [] _ = False
    astEq _ [] = False
    astEq (t1:ts1) (t2:ts2) = astEq t1 t2 && astEq ts1 ts2

data Decl'           = DeclI IncludeDecl
                     | DeclC ClassDecl
                     | DeclV VarDecl
                     | DeclF FunDecl
                       deriving (Eq, Show)
type Decl            = (Decl', Meta)

instance PrettyPrint Decl where
    prettyPrint (DeclI i, _) = prettyPrint i
    prettyPrint (DeclC c, _) = prettyPrint c
    prettyPrint (DeclV v, _) = prettyPrint v
    prettyPrint (DeclF f, _) = prettyPrint f ++ "\n"

instance (ASTEq Decl) where
    astEq (DeclI i1, _) (DeclI i2, _) = astEq i1 i2
    astEq (DeclC c1, _) (DeclC c2, _) = astEq c1 c2
    astEq (DeclV v1, _) (DeclV v2, _) = astEq v1 v2
    astEq (DeclF f1, _) (DeclF f2, _) = astEq f1 f2
    astEq _ _ = False

data IncludeDecl'    = IncludeDecl String
                       deriving (Eq, Show)
type IncludeDecl     = (IncludeDecl', Meta)

instance PrettyPrint IncludeDecl where
    prettyPrint (IncludeDecl filePath, _) = "#include \"" ++ filePath ++ "\""

instance (ASTEq IncludeDecl) where
    astEq (IncludeDecl s1, _) (IncludeDecl s2, _) = s1 == s2

data ClassDecl'      = ClassDecl ClassIdentifier [VarDecl] [FunDecl]
                       deriving (Eq, Show)
type ClassDecl       = (ClassDecl', Meta)

instance PrettyPrint ClassDecl where
    prettyPrint (ClassDecl i varDecls funDecls, _) = prettyPrint i
        ++ "\n{\n"
        ++ indent (prettyPrint varDecls)
        ++ "\n\n"
        ++ indent (prettyPrint funDecls) ++ "\n}"

instance (ASTEq ClassDecl) where
    astEq (ClassDecl i1 vs1 fs1, _) (ClassDecl i2 vs2 fs2, _) = astEq i1 i2 && astEq vs1 vs2 && astEq fs1 fs2

data VarDecl'        = VarDeclTyped Type Identifier Expression
                     | VarDeclUntyped Identifier Expression
                     | VarDeclTypedUnitialized Type Identifier
                     | VarDeclUntypedUnitialized Identifier
                       deriving (Eq, Show)
type VarDecl         = (VarDecl', Meta)

instance PrettyPrint VarDecl where
    prettyPrint (VarDeclTyped t i e, _) = prettyPrint t ++ " " ++ prettyPrint i ++ " = " ++ prettyPrint e ++ ";"
    prettyPrint (VarDeclUntyped i e, _) = "var " ++ prettyPrint i ++ " = " ++ prettyPrint e ++ ";"
    prettyPrint (VarDeclTypedUnitialized t i, _) = prettyPrint t ++ " " ++ prettyPrint i ++ ";"
    prettyPrint (VarDeclUntypedUnitialized i, _) = "var " ++ prettyPrint i ++ ";"

instance (ASTEq VarDecl) where
    astEq (VarDeclTyped t1 i1 e1, _) (VarDeclTyped t2 i2 e2, _) = astEq t1 t2 && astEq i1 i2 && astEq e1 e2
    astEq (VarDeclUntyped i1 e1, _) (VarDeclUntyped i2 e2, _) = astEq i1 i2 && astEq e1 e2
    astEq (VarDeclTypedUnitialized t1 i1, _) (VarDeclTypedUnitialized t2 i2, _) = astEq t1 t2 && astEq i1 i2
    astEq (VarDeclUntypedUnitialized i1, _) (VarDeclUntypedUnitialized i2, _) = astEq i1 i2
    astEq _ _ = False

data FunDecl'        = FunDeclTyped Identifier [Identifier] Type [Statement] -- [Identifier] are the arguments
                     | FunDeclUntyped Identifier [Identifier] [Statement]
                       deriving (Eq, Show)
type FunDecl         = (FunDecl', Meta)

instance PrettyPrint FunDecl where
    prettyPrint (f, _) = case f of
        FunDeclTyped name args fType ss ->
            prettyPrint name
            ++ " (" ++ printArgs 0 args ++ ") :: " ++ prettyPrint fType ++ "\n{\n"
            ++ indent (prettyPrint ss) ++ "\n"
            ++ "}"
        FunDeclUntyped name args ss ->
            prettyPrint name
            ++ " (" ++ printArgs 0 args ++ ")\n{\n"
            ++ indent (prettyPrint ss) ++ "\n"
            ++ "}"
        where
            printArgs :: Int -> [Identifier] -> String
            printArgs _ [] = ""
            printArgs 0 (arg:args) = prettyPrint arg ++ printArgs 1 args
            printArgs n (arg:args) = ", " ++ prettyPrint arg ++ printArgs 2 args

instance (ASTEq FunDecl) where
    astEq (FunDeclTyped i1 is1 t1 ss1, _) (FunDeclTyped i2 is2 t2 ss2, _) = astEq i1 i2 && astEq is1 is2 && astEq t1 t2 && astEq ss1 ss2
    astEq (FunDeclUntyped i1 is1 ss1, _) (FunDeclUntyped i2 is2 ss2, _) = astEq i1 i2 && astEq is1 is2 && astEq ss1 ss2
    astEq _ _ = False

data Type'           = TypeVoid
                     | TypeTuple Type Type
                     | TypeList Type
                     | TypeInt
                     | TypeBool
                     | TypeChar
                     | TypeIdentifier Identifier
                     | TypePointer Type
                     | TypeFunction [Type] Type
                     | TypeType
                     | TypeClass ClassIdentifier
                       deriving (Eq, Show)
type Type            = (Type', Meta)

instance PrettyPrint Type where
    prettyPrint (TypeVoid, _) = "Void"
    prettyPrint (TypeTuple t1 t2, _) = "(" ++ prettyPrint t1 ++ "," ++ prettyPrint t2 ++ ")"
    prettyPrint (TypeList t, _) = "[" ++ prettyPrint t ++ "]"
    prettyPrint (TypeInt, _) = "Int"
    prettyPrint (TypeBool, _) = "Bool"
    prettyPrint (TypeChar, _) = "Char"
    prettyPrint (TypeIdentifier i, _) = prettyPrint i
    prettyPrint (TypePointer t, _) = prettyPrint t ++ "*"
    prettyPrint (TypeFunction argTypes returnType, _) = printArgsTypes argTypes ++ "-> " ++ prettyPrint returnType
        where
            printArgsTypes :: [Type] -> String
            printArgsTypes [] = ""
            printArgsTypes (arg:args) = prettyPrint arg ++ " " ++ printArgsTypes args
    prettyPrint (TypeType, _) = "Type"
    prettyPrint (TypeClass i, _) = prettyPrint i

instance (ASTEq Type) where
    astEq (TypeVoid, _) (TypeVoid, _) = True
    astEq (TypeTuple t1 t1', _) (TypeTuple t2 t2', _) = astEq t1 t2 && astEq t1' t2'
    astEq (TypeList t1, _) (TypeList t2, _) = astEq t1 t2
    astEq (TypeInt, _) (TypeInt, _) = True
    astEq (TypeBool, _) (TypeBool, _) = True
    astEq (TypeChar, _) (TypeChar, _) = True
    astEq (TypeIdentifier i1, _) (TypeIdentifier i2, _) = astEq i1 i2
    astEq (TypePointer t1, _) (TypePointer t2, _) = astEq t1 t2
    astEq (TypeFunction ts1 t1, _) (TypeFunction ts2 t2, _) = astEq ts1 ts2 && astEq t1 t2
    astEq (TypeType, _) (TypeType, _) = True
    astEq (TypeClass i1, _) (TypeClass i2, _) = astEq i1 i2
    astEq _ _ = False

data Statement'      = StmtVarDecl VarDecl
                     | StmtIf Expression Statement
                     | StmtIfElse Expression Statement Statement
                     | StmtWhile Expression Statement
                     | StmtBlock [Statement]
                     | StmtAssignment Expression Expression -- lhs must have value type PermanentValue
                     | StmtFunCall Expression [Expression] --lhs must be of type TypeFunction
                     | StmtReturn Expression
                     | StmtReturnVoid
                     | StmtDelete Expression
                       deriving (Eq, Show)
type Statement       = (Statement', Meta)

indentIfNotBlock :: Statement -> String
indentIfNotBlock (StmtBlock s, p) = prettyPrint (StmtBlock s, p)
indentIfNotBlock s = indent $ prettyPrint s

instance PrettyPrint Statement where
    prettyPrint (StmtVarDecl v, _) = prettyPrint v
    prettyPrint (StmtIf e s, _) = "if ("
        ++ prettyPrint e
        ++ ")\n"
        ++ indentIfNotBlock s
    prettyPrint (StmtIfElse e s1 s2, _) = "if ("
        ++ prettyPrint e
        ++ ")\n"
        ++ indentIfNotBlock s1
        ++ "\nelse\n"
        ++ indentIfNotBlock s2
    prettyPrint (StmtWhile e s, _) = "while ("
        ++ prettyPrint e
        ++ ")"
        ++ indentIfNotBlock s
    prettyPrint (StmtBlock ss, _) = "{\n"
        ++ indent (prettyPrint ss)
        ++ "\n}"
    prettyPrint (StmtAssignment e1 e2, _) = prettyPrint e1 ++ " = " ++ prettyPrint e2 ++ ";"
    prettyPrint (StmtFunCall e es, _) = prettyPrint e ++ "(" ++ printArgs 0 es ++ ");"
        where
            printArgs :: Int -> [Expression] -> String
            printArgs _ [] = ""
            printArgs 0 (e:es) = prettyPrint e ++ printArgs 1 es
            printArgs n (e:es) = ", " ++ prettyPrint e ++ printArgs (n+1) es
    prettyPrint (StmtReturn e, _) = "return " ++ prettyPrint e ++ ";"
    prettyPrint (StmtReturnVoid, _) = "return;"
    prettyPrint (StmtDelete e, _) = "delete " ++ prettyPrint e ++ ";"

instance (ASTEq Statement) where
    astEq (StmtVarDecl v1, _) (StmtVarDecl v2, _) = astEq v1 v2
    astEq (StmtIf e1 s1, _) (StmtIf e2 s2, _) = astEq e1 e2 && astEq s1 s2
    astEq (StmtIfElse e1 s1 s1', _) (StmtIfElse e2 s2 s2', _) = astEq e1 e2 && astEq s1 s2 && astEq s1' s2'
    astEq (StmtWhile e1 s1, _) (StmtWhile e2 s2, _) = astEq e1 e2 && astEq s1 s2
    astEq (StmtBlock ss1, _) (StmtBlock ss2, _) = astEq ss1 ss2
    astEq (StmtAssignment e1 e1', _) (StmtAssignment e2 e2', _) = astEq e1 e2 && astEq e1' e2'
    astEq (StmtFunCall e1 es1, _) (StmtFunCall e2 es2, _) = astEq e1 e2 && astEq es1 es2
    astEq (StmtReturn e1, _) (StmtReturn e2, _) = astEq e1 e2
    astEq (StmtReturnVoid, _) (StmtReturnVoid, _) = True
    astEq (StmtDelete e1, _) (StmtDelete e2, _) = astEq e1 e2
    astEq _ _ = False

data Expression'     = ExprIdentifier Identifier
                     | ExprField Expression [Field]
                     | ExprFunCall Expression [Expression]
                     | ExprConstant Constant
                     | ExprTuple Expression Expression
                     | ExprUnaryOp UnaryOperator Expression
                     | ExprBinaryOp BinaryOperator Expression Expression
                     | ExprNew ClassIdentifier
                     | ExprDelete Expression
                     | ExprClassMember Expression Identifier
                       deriving (Eq, Show)
type Expression      = (Expression', Meta)

instance PrettyPrint Expression where
    prettyPrint (ExprIdentifier i, _) = prettyPrint i
    prettyPrint (ExprField e field, _) = prettyPrint e ++ printField field
        where
            printField :: [Field] -> String
            printField = foldr ((++) . prettyPrint) ""
    prettyPrint (ExprFunCall e es, _) = prettyPrint e ++ "(" ++ printArgs 0 es ++ ")"
        where
            printArgs :: Int -> [Expression] -> String
            printArgs _ [] = ""
            printArgs 0 (e:es) = prettyPrint e ++ printArgs 1 es
            printArgs n (e:es) = ", " ++ prettyPrint e ++ printArgs (n+1) es
    prettyPrint (ExprConstant c, _) = prettyPrint c
    prettyPrint (ExprTuple e1 e2, _) = "(" ++ prettyPrint e1 ++ ", " ++ prettyPrint e2 ++ ")"
    prettyPrint (ExprUnaryOp op e, _) = prettyPrint op ++ prettyPrint e
    prettyPrint (ExprBinaryOp op e1 e2, _) = case binaryOperatorAssociativity op of
        ALeft ->
            print' (binaryOperatorPrecedence op - 1) e1
            ++ " " ++ prettyPrint op ++ " "
            ++ print' (binaryOperatorPrecedence op) e2
        ARight ->
            print' (binaryOperatorPrecedence op) e1
            ++ " " ++ prettyPrint op ++ " "
            ++ print' (binaryOperatorPrecedence op - 1) e2
        where
            print' :: Int -> Expression -> String
            print' precedence e = case e of
                (ExprBinaryOp op' _ _, _) -> if binaryOperatorPrecedence op' <= precedence
                    then "(" ++ prettyPrint e ++ ")"
                    else prettyPrint e
                _ -> prettyPrint e
    prettyPrint (ExprNew i, _) = "new " ++ prettyPrint i
    prettyPrint (ExprDelete i, _) = "delete " ++ prettyPrint i
    prettyPrint (ExprClassMember e i, _) = case e of
        (ExprUnaryOp _ _, _) -> "(" ++ prettyPrint e ++ ")"
        _ -> prettyPrint e
        ++ "." ++ prettyPrint i

instance (ASTEq Expression) where
    astEq (ExprIdentifier i1, _) (ExprIdentifier i2, _) = astEq i1 i2
    astEq (ExprField e1 f1, _) (ExprField e2 f2, _) = astEq e1 e2 && astEq f1 f2
    astEq (ExprFunCall e1 es1, _) (ExprFunCall e2 es2, _) = astEq e1 e2 && astEq es1 es2
    astEq (ExprConstant c1, _) (ExprConstant c2, _) = astEq c1 c2
    astEq (ExprTuple t1 t1', _) (ExprTuple t2 t2', _) = astEq t1 t2 && astEq t2 t2'
    astEq (ExprUnaryOp uOp1 e1, _) (ExprUnaryOp uOp2 e2, _) = astEq uOp1 uOp2 && astEq e1 e2
    astEq (ExprBinaryOp bOp1 e1 e1', _) (ExprBinaryOp bOp2 e2 e2', _) = astEq bOp1 bOp2 && astEq e1 e2 && astEq e1' e2'
    astEq (ExprNew i1, _) (ExprNew i2, _) = astEq i1 i2
    astEq (ExprDelete i1, _) (ExprDelete i2, _) = astEq i1 i2
    astEq (ExprClassMember e1 i1, _) (ExprClassMember e2 i2, _) = astEq e1 e2 && astEq i1 i2
    astEq _ _ = False

data Constant'       = ConstBool Bool
                     | ConstInt Int
                     | ConstChar Char
                     | ConstEmptyList
                       deriving (Eq, Show)
type Constant        = (Constant', Meta)

instance PrettyPrint Constant where
    prettyPrint (ConstBool b, _) = show b
    prettyPrint (ConstInt i, _) = show i
    prettyPrint (ConstChar c, _) = show c
    prettyPrint (ConstEmptyList, _) = "[]"

instance (ASTEq Constant) where
    astEq (ConstBool b1, _) (ConstBool b2, _) = b1 == b2
    astEq (ConstInt i1, _) (ConstInt i2, _) = i1 == i2
    astEq (ConstChar c1, _) (ConstChar c2, _) = c1 == c2
    astEq (ConstEmptyList, _) (ConstEmptyList, _) = True
    astEq _ _ = False

data UnaryOperator'  = UnaryOpNeg
                     | UnaryOpBitwiseNot
                     | UnaryOpSubtr
                     | UnaryOpCast Type
                     | UnaryOpReference
                     | UnaryOpDereference
                       deriving (Eq, Show)
type UnaryOperator   = (UnaryOperator', Meta)

instance PrettyPrint UnaryOperator where
    prettyPrint (UnaryOpNeg, _) = "!"
    prettyPrint (UnaryOpBitwiseNot, _) = "~"
    prettyPrint (UnaryOpSubtr, _) = "-"
    prettyPrint (UnaryOpCast t, _) = "(" ++ prettyPrint t ++ ")"
    prettyPrint (UnaryOpReference, _) = "&"
    prettyPrint (UnaryOpDereference, _) = "*"

instance (ASTEq UnaryOperator) where
    astEq (uOp1, _) (uOp2, _) = uOp1 == uOp2

data BinaryOperator' = BinaryOpOr
                     | BinaryOpBitwiseOr
                     | BinaryOpAnd
                     | BinaryOpBitwiseAnd
                     | BinaryOpEq
                     | BinaryOpNEq
                     | BinaryOpLT
                     | BinaryOpGT
                     | BinaryOpLTE
                     | BinaryOpGTE
                     | BinaryOpConcat
                     | BinaryOpPlus
                     | BinaryOpSubtr
                     | BinaryOpReferencePlus
                     | BinaryOpReferenceSubtr
                     | BinaryOpReferenceReferenceSubtr
                     | BinaryOpMult
                     | BinaryOpDiv
                     | BinaryOpMod
                     | BinaryOpBitShiftLeft
                     | BinaryOpBitShiftRight
                       deriving (Eq, Show)
type BinaryOperator  = (BinaryOperator', Meta)

instance PrettyPrint BinaryOperator where
    prettyPrint (BinaryOpOr, _) = "||"
    prettyPrint (BinaryOpBitwiseOr, _) = "|"
    prettyPrint (BinaryOpAnd, _) = "&&"
    prettyPrint (BinaryOpBitwiseAnd, _) = "&"
    prettyPrint (BinaryOpEq, _) = "=="
    prettyPrint (BinaryOpNEq, _) = "!="
    prettyPrint (BinaryOpLT, _) = "<"
    prettyPrint (BinaryOpGT, _) = ">"
    prettyPrint (BinaryOpLTE, _) = "<="
    prettyPrint (BinaryOpGTE, _) = ">="
    prettyPrint (BinaryOpConcat, _) = ":"
    prettyPrint (BinaryOpPlus, _) = "+"
    prettyPrint (BinaryOpSubtr, _) = "-"
    prettyPrint (BinaryOpReferencePlus, _) = "&+"
    prettyPrint (BinaryOpReferenceSubtr, _) = "&-"
    prettyPrint (BinaryOpReferenceReferenceSubtr, _) = "&-&"
    prettyPrint (BinaryOpMult, _) = "*"
    prettyPrint (BinaryOpDiv, _) = "/"
    prettyPrint (BinaryOpMod, _) = "%"
    prettyPrint (BinaryOpBitShiftLeft, _) = "<<"
    prettyPrint (BinaryOpBitShiftRight, _) = ">>"

instance (ASTEq BinaryOperator) where
    astEq (bOp1, _) (bOp2, _) = bOp1 == bOp2

data Field'          = FieldHd
                     | FieldTl
                     | FieldFst
                     | FieldSnd
                       deriving (Eq, Show)
type Field           = (Field', Meta)

instance PrettyPrint Field where
    prettyPrint (FieldHd, _) = ".hd"
    prettyPrint (FieldTl, _) = ".tl"
    prettyPrint (FieldFst, _) = ".fst"
    prettyPrint (FieldSnd, _) = ".snd"

instance (ASTEq Field) where
    astEq (f1, _) (f2, _) = f1 == f2

newtype Identifier'  = Identifier String
                       deriving (Ord, Eq, Show)
type Identifier      = (Identifier', Meta)

instance PrettyPrint Identifier where
    prettyPrint (Identifier i, _) = i

instance (ASTEq Identifier) where
    astEq (Identifier s1, _) (Identifier s2, _) = s1 == s2

newtype ClassIdentifier' = ClassIdentifier String
                           deriving (Ord, Eq, Show)
type ClassIdentifier     = (ClassIdentifier', Meta)

instance PrettyPrint ClassIdentifier where
    prettyPrint (ClassIdentifier i, _) = i

instance ASTEq ClassIdentifier where
    astEq (ClassIdentifier s1, _) (ClassIdentifier s2, _) = s1 == s2

binaryOperatorPrecedence' :: BinaryOperator' -> Int
binaryOperatorPrecedence' BinaryOpOr = 1
binaryOperatorPrecedence' BinaryOpBitwiseOr = 1
binaryOperatorPrecedence' BinaryOpAnd = 2
binaryOperatorPrecedence' BinaryOpBitwiseAnd = 2
binaryOperatorPrecedence' BinaryOpEq = 3
binaryOperatorPrecedence' BinaryOpNEq = 3
binaryOperatorPrecedence' BinaryOpLT = 4
binaryOperatorPrecedence' BinaryOpGT = 4
binaryOperatorPrecedence' BinaryOpLTE = 4
binaryOperatorPrecedence' BinaryOpGTE = 4
binaryOperatorPrecedence' BinaryOpConcat = 5
binaryOperatorPrecedence' BinaryOpPlus = 6
binaryOperatorPrecedence' BinaryOpSubtr = 6
binaryOperatorPrecedence' BinaryOpReferencePlus = 5
binaryOperatorPrecedence' BinaryOpReferenceSubtr = 5
binaryOperatorPrecedence' BinaryOpReferenceReferenceSubtr = 5
binaryOperatorPrecedence' BinaryOpMult = 7
binaryOperatorPrecedence' BinaryOpDiv = 7
binaryOperatorPrecedence' BinaryOpMod = 7
binaryOperatorPrecedence' BinaryOpBitShiftLeft = 5
binaryOperatorPrecedence' BinaryOpBitShiftRight = 5

binaryOperatorPrecedence :: BinaryOperator -> Int
binaryOperatorPrecedence (bOp, _) = binaryOperatorPrecedence' bOp

data Associativity = ALeft | ARight
                     deriving (Eq)

binaryOperatorAssociativity' :: BinaryOperator' -> Associativity
binaryOperatorAssociativity' BinaryOpOr = ALeft
binaryOperatorAssociativity' BinaryOpBitwiseOr = ALeft
binaryOperatorAssociativity' BinaryOpAnd = ALeft
binaryOperatorAssociativity' BinaryOpBitwiseAnd = ALeft
binaryOperatorAssociativity' BinaryOpEq = ALeft
binaryOperatorAssociativity' BinaryOpNEq = ALeft
binaryOperatorAssociativity' BinaryOpLT = ALeft
binaryOperatorAssociativity' BinaryOpGT = ALeft
binaryOperatorAssociativity' BinaryOpLTE = ALeft
binaryOperatorAssociativity' BinaryOpGTE = ALeft
binaryOperatorAssociativity' BinaryOpConcat = ARight
binaryOperatorAssociativity' BinaryOpPlus = ALeft
binaryOperatorAssociativity' BinaryOpSubtr = ALeft
binaryOperatorAssociativity' BinaryOpReferencePlus = ALeft
binaryOperatorAssociativity' BinaryOpReferenceSubtr = ALeft
binaryOperatorAssociativity' BinaryOpReferenceReferenceSubtr = ALeft
binaryOperatorAssociativity' BinaryOpMult = ALeft
binaryOperatorAssociativity' BinaryOpDiv = ALeft
binaryOperatorAssociativity' BinaryOpMod = ALeft
binaryOperatorAssociativity' BinaryOpBitShiftLeft = ALeft
binaryOperatorAssociativity' BinaryOpBitShiftRight = ALeft

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
