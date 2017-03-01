module Lexer.Lexer where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Except
import Data.List.Extras.Argmax
import qualified Text.Regex.Thompson as Regex

data Pos = Pos { line :: Int, column :: Int }

------------------------------------------------------------------------------------------------------------------------
-- Data types

instance Show Pos where
    show (Pos line column) = "(" ++ show line ++ "," ++ show column ++ ")"

data Token = TKeyword Keyword
           | TIdentifier Identifier
           | TConstant Constant
           | TType Type
           | TOperator Operator
           | TField Field
           | TPunctuator Punctuator
           | TWhitespace Whitespace
           | TEOF
             deriving (Show, Eq)

data TokenP = TP { token :: Token, pos :: Pos }

data Operator = OPlus
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

data Punctuator = PParenOpen
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

------------------------------------------------------------------------------------------------------------------------
-- Scanner

data LexError = UnexpectedToken Token Token
                 deriving (Eq)
type LexErrorP = (LexError, Pos)
type LexT = StateT (String, Pos) (Except LexErrorP)

type Recognizer = String -> Maybe (String, Token)
data RecognizerPriority = RP {recognizer :: Recognizer, priority :: Int}

{-|
Constructs a RecognizerPriority, the recognizer of which takes an input string, and outputs the recognized string and
the corresponding token.

The recognizer is built by taking a priority, the regex string, and function to turn a string into a token.
-}
constructRecognizer :: Int -> String -> (String -> Token) -> RecognizerPriority
constructRecognizer priority regexString stringToToken = RP recognizer' priority
    where
    re = Regex.compile regexString
    recognizer' :: Recognizer
    recognizer' str =
        case match of
            Just matchStr -> Just (matchStr, stringToToken matchStr)
            Nothing -> Nothing
        where
        match = Regex.longestMatch re str

recognizers :: [RecognizerPriority]
recognizers = [constructRecognizer 6 "" (\s -> TEOF),
               constructRecognizer 5 "\r?\n" (\s -> TWhitespace WNewline),
               constructRecognizer 5 "[ \f\t\v]+" (\s -> TWhitespace WOther),
               constructRecognizer 4 "var" (\s -> TKeyword KVar),
               constructRecognizer 4 "if" (\s -> TKeyword KIf),
               constructRecognizer 4 "else" (\s -> TKeyword KElse),
               constructRecognizer 4 "while" (\s -> TKeyword KWhile),
               constructRecognizer 4 "return" (\s -> TKeyword KReturn),
               constructRecognizer 0 "'[^']'|'\\''" (\s -> TConstant (CChar 'a')),
               constructRecognizer 0 "[0-9]+" (\s -> TConstant (CInt 5))
               ]

{-
Recognize an input string. Uses all input recognizers on the input string.

First, the outputs of all recognizers that recognized the longest input string is used.

To tie-break, the output of a single recognizer that has the highest priority is used.
-}
recognize :: [RecognizerPriority] -> String -> Maybe (String, Token)
recognize recognizers input = fst $ argmax (
        \(_, priority) -> priority
    ) maxRecognizers
    where
        -- |The output of recognizers (and their priority)
        recognizersApplied :: [(Maybe (String, Token), Int)]
        recognizersApplied = [(recognizer input, priority) | RP recognizer priority <- recognizers]

        -- |The output of recognizers (and their priority) that have recognized the longest string
        maxRecognizers :: [(Maybe (String, Token), Int)]
        maxRecognizers = argmaxes (
                \recognized ->
                    case recognized of
                        (Just (matchStr, _), _) -> length matchStr
                        (Nothing, _) -> -1
            ) recognizersApplied

-- |Scan an input string to a list of tokens
lex :: String -> [Token]
lex = undefined
