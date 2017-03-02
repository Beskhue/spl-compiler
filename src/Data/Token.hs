{-|
Module: Token
Description: Data types for tokens.
Copyright: (c) Thomas Churchman, 2017
License: MIT
Maintainer: thomas@kepow.org
Stability: experimental
-}

module Data.Token where

import Data.Pos

data Token = TKeyword Keyword
           | TIdentifier Identifier
           | TConstant Constant
           | TType Type
           | TOperator Operator
           | TField Field
           | TPunctuator Punctuator
           | TWhitespace Whitespace
           | TComment Int -- Int encodes how many lines the comment spans
           | TEOF
             deriving (Show, Eq)

data TokenP = TP { token :: Token, pos :: Pos }

instance Show TokenP where
    show (TP token pos) = show token ++ ":" ++ show pos

data Operator = OAssignment
              | OPlus
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

data Constant = CBool Bool
              | CInt Int
              | CChar Char
              | CEmptyList
                deriving (Show, Eq)

data Field = FHd
           | FTl
           | FFst
           | FSnd
             deriving (Show, Eq)

data Punctuator = PSeparator
                | PParenOpen
                | PParenClose
                | PBraceOpen
                | PBraceClose
                | PSquareBracketOpen
                | PSquareBracketClose
                | PComma
                | PMapTo
                | PMinus
                  deriving (Show, Eq)

data Whitespace = WNewline
                | WOther
                  deriving (Show, Eq)

type Identifier = String
