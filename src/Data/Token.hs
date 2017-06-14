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
           | TClassIdentifier Identifier
           | TConstant Constant
           | TType Type
           | TOperator Operator
           | TField Field
           | TPunctuator Punctuator
           | TWhitespace Whitespace
           | TComment Int -- Int encodes how many lines the comment spans
           | TInclude String
           | TEOF
             deriving (Show, Eq)

data TokenP = TP { token :: Token, pos :: Pos }
              deriving (Eq)

getToken :: TokenP -> Token
getToken (TP t _) = t

getTokens :: [TokenP] -> [Token]
getTokens = map getToken

instance Show TokenP where
    show (TP token pos) = show token ++ ":" ++ show pos

data Operator = OAssignment
              | OPlus
              | OReferencePlus
              | OReferenceMinus
              | OReferenceReferenceMinus
              | ODivide
              | OMod
              | OEq
              | OLT
              | OGT
              | OLTE
              | OGTE
              | ONEq
              | OAnd
              | OConcat
              | ONeg
              | OBitShiftLeft
              | OBitShiftRight
              | ODot
                deriving (Show, Eq)

data Keyword = KVar
             | KIf
             | KElse
             | KWhile
             | KReturn
             | KClass
             | KNew
             | KDelete
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
                | PFunType
                | PMinus
                | PAsterisk
                | PAmpersand
                | PPipe
                | PTilde
                  deriving (Show, Eq)

data Whitespace = WNewline
                | WOther
                  deriving (Show, Eq)

type Identifier = String
