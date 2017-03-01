module Lexer.Lexer where

type Pos = { line :: Int, column :: Int }

------------------------------------------------------------------------------------------------------------------------
-- Data types

instance Show Pos where
    show (Pos line column) = "(" ++ show line ++ "," ++ show colum ++ ")"

data Token = TOperator Operator
           | TKeyword Keyword
           | TType Type
           | TValue Value
           | TIdentifier Identifier
           | TField Field
           | TParenOpen
           | TParenClose
           | TBraceOpen
           | TBraceClose
           | TSquareBracketOpen
           | TSquareBracketClose
           | TComma
           | TMapTo
             deriving (Show, Eq)

type TokenP = Token Pos

data Operator = OPlus
              | OMin
              | OMultiply
              | ODivide
              | OMod
              | OEq
              | OLT
              | OGT
              | OLTE
              | OGTE
              | ONEq
              | OAnd
              | OOr
              | OConcat
              | ONeg
              | ONumNeg
                deriving (Show, Eq)

data Keyword = KVar
             | KIf
             | KElse
             | KWhile
             | KReturn
               deriving (Show, Eq)

data Type = TypeInt
          | TypeBool
          | TypeChar
          | TypeVoid
            deriving (Show, Eq)

data Value = VTrue
           | VFalse
           | VEmptyList
           | VInt Int

data Field = FHd
           | FTl
           | FFst
           | FSnd
             deriving (Show, Eq)

type Identifier = String

------------------------------------------------------------------------------------------------------------------------
-- Scanner

-- |Scan an input string to a list of tokens
lex :: String -> [Token]
lex = undefined

