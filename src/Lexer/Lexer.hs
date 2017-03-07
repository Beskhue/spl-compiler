module Lexer.Lexer where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Except
import Data.List.Extras.Argmax
import Data.Token
import Data.Pos
import qualified Text.Regex.Thompson as Regex

------------------------------------------------------------------------------------------------------------------------
-- Token recognizer

type Recognizer = String -> Maybe (String, Token)
data RecognizerPriority = RP {recognizer :: Recognizer, priority :: Int}

{-|
Constructs a RecognizerPriority, the recognizer of which takes an input string, and outputs the longest recognized
string and the corresponding token.

The recognizer is built by taking a priority, the regex string, and function to turn a string into a token.
-}
constructRecognizer :: Int -> String -> (String -> Token) -> RecognizerPriority
constructRecognizer = constructRecognizer' True

-- |Same as above, except it output the shortest recognized string
constructShortestRecognizer :: Int -> String -> (String -> Token) -> RecognizerPriority
constructShortestRecognizer = constructRecognizer' False

constructRecognizer' :: Bool -> Int -> String -> (String -> Token) -> RecognizerPriority
constructRecognizer' recognizeLongestString priority regexString stringToToken = RP recognizer' priority
    where
    re = Regex.compile regexString
    recognizer' :: Recognizer
    recognizer' str =
        case match of
            Just matchStr -> Just (matchStr, stringToToken matchStr)
            Nothing -> Nothing
        where
        match = if recognizeLongestString
            then Regex.longestMatch re str
            else Regex.shortestMatch re str

recognizers :: [RecognizerPriority]
recognizers = [ -- End of file
                constructRecognizer 20 "$" (\s -> TEOF),
                -- Comments
                constructShortestRecognizer 15 "/\\*.*\\*/" (\s -> TComment $ length $ filter (== '\n') s),
                constructRecognizer 15 "//[^\r\n]*" (\s -> TComment 0),
                -- Line ends
                constructRecognizer 14 "\r?\n" (\s -> TWhitespace WNewline),
                -- Whitespace
                constructRecognizer 14 "[ \f\t\v]+" (\s -> TWhitespace WOther),
                -- Punctuators
                constructRecognizer 13 ";" (\s -> TPunctuator PSeparator),
                constructRecognizer 13 "\\(" (\s -> TPunctuator PParenOpen),
                constructRecognizer 13 "\\)" (\s -> TPunctuator PParenClose),
                constructRecognizer 13 "{" (\s -> TPunctuator PBraceOpen),
                constructRecognizer 13 "}" (\s -> TPunctuator PBraceClose),
                constructRecognizer 13 "\\[" (\s -> TPunctuator PSquareBracketOpen),
                constructRecognizer 13 "\\]" (\s -> TPunctuator PSquareBracketClose),
                constructRecognizer 13 "," (\s -> TPunctuator PComma),
                constructRecognizer 13 "\\->" (\s -> TPunctuator PMapTo),
                constructRecognizer 13 "\\-" (\s -> TPunctuator PMinus),
                constructRecognizer 13 "::" (\s -> TPunctuator PFunType),
                -- Keywords
                constructRecognizer 10 "var" (\s -> TKeyword KVar),
                constructRecognizer 10 "if" (\s -> TKeyword KIf),
                constructRecognizer 10 "else" (\s -> TKeyword KElse),
                constructRecognizer 10 "while" (\s -> TKeyword KWhile),
                constructRecognizer 10 "return" (\s -> TKeyword KReturn),
                -- Types
                constructRecognizer 9 "Int" (\s -> TType TypeInt),
                constructRecognizer 9 "Bool" (\s -> TType TypeBool),
                constructRecognizer 9 "Char" (\s -> TType TypeChar),
                constructRecognizer 9 "Void" (\s -> TType TypeVoid),
                -- Operators
                constructRecognizer 8 "=" (\s -> TOperator OAssignment),
                constructRecognizer 8 "\\+" (\s -> TOperator OPlus),
                constructRecognizer 8 "\\*" (\s -> TOperator OMultiply),
                constructRecognizer 8 "/" (\s -> TOperator ODivide),
                constructRecognizer 8 "%" (\s -> TOperator OMod),
                constructRecognizer 8 "==" (\s -> TOperator OEq),
                constructRecognizer 8 "<" (\s -> TOperator OLT),
                constructRecognizer 8 ">" (\s -> TOperator OGT),
                constructRecognizer 8 "<=" (\s -> TOperator OLTE),
                constructRecognizer 8 ">=" (\s -> TOperator OGTE),
                constructRecognizer 8 "!=" (\s -> TOperator ONEq),
                constructRecognizer 8 "&&" (\s -> TOperator OAnd),
                constructRecognizer 8 "\\|\\|" (\s -> TOperator OOr),
                constructRecognizer 8 ":" (\s -> TOperator OConcat),
                constructRecognizer 8 "!" (\s -> TOperator ONeg),
                -- Fields
                constructRecognizer 7 "\\.hd" (\s -> TField FHd),
                constructRecognizer 7 "\\.tl" (\s -> TField FTl),
                constructRecognizer 7 "\\.fst" (\s -> TField FFst),
                constructRecognizer 7 "\\.snd" (\s -> TField FSnd),
                -- Constants
                constructRecognizer 1 "True" (\s -> TConstant $ CBool True),
                constructRecognizer 1 "False" (\s -> TConstant $ CBool False),
                constructRecognizer 1 "[0-9]+" (\s -> TConstant $ CInt (read s :: Int)),
                constructRecognizer 1 "0x[0-9a-fA-F]+" (\s -> TConstant $ CInt (read s :: Int)), -- Hex
                constructRecognizer 1 "'[^']'|'\\\\''" (\s -> TConstant $ CChar $ s !! (length s - 2)),
                constructRecognizer 1 "\\[\\]" (\s -> TConstant CEmptyList),
                -- Identifiers
                constructRecognizer 0 "[a-zA-Z]([_a-zA-Z0-9])*" (\s -> TIdentifier s)
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

------------------------------------------------------------------------------------------------------------------------
-- Scanner

data LexError = UnrecognizedCharacter Char
                deriving (Eq)
type LexErrorP = (LexError, Pos)
type LexT = StateT (TokenP, String, Pos) (Except LexErrorP)

instance Show LexError where
    show (UnrecognizedCharacter c) = "Unrecognized character: " ++ [c]

-- |Scan an input string to either a lexing error or a list of tokens
lex :: String -> String -> Either LexErrorP [TokenP]
lex fileName str = case runExcept $ evalStateT lexSteps (undefined, str, Pos fileName 1 1) of
    Left lexError -> Left lexError
    Right ts -> Right $ filterWhitespaceAndComments ts

filterWhitespaceAndComments :: [TokenP] -> [TokenP]
filterWhitespaceAndComments = filter
    (
        \tp -> case tp of
            TP (TWhitespace _) _ -> False
            TP (TComment _) _ -> False
            _ -> True
    )

-- |Get the current lexeme from the state
getLexeme :: LexT TokenP
getLexeme = do
    (lexeme, _, _) <- get
    return lexeme

-- |Get the string from the state
getStr :: LexT String
getStr = do
    (_, str, _) <- get
    return str

-- |Get the position from the state
getPos :: LexT Pos
getPos = do
    (_, _, pos) <- get
    return pos

lexSteps :: LexT [TokenP]
lexSteps = do
    advance
    lexeme <- getLexeme
    case lexeme of
        TP TEOF _ -> return [lexeme]
        _ -> liftM (lexeme :) lexSteps

advance :: LexT ()
advance = do
    str <- getStr
    pos <- getPos
    case recognize recognizers str of
        Nothing -> do
            put (undefined, str, pos)
            lift $ throwE (UnrecognizedCharacter (head str), pos)
        Just (recognizedStr, t) ->
            let newStr = drop (length recognizedStr) str in
                case t of
                    TWhitespace WNewline -> put (
                            TP t pos,
                            newStr,
                            (flip setColumn) 1 (increaseLine pos 1)
                        )
                    TComment n -> put (
                            TP t pos,
                            newStr,
                            (flip setColumn) (countCharsFromLastNewline recognizedStr + 1) (increaseLine pos n)
                            -- Pos (line + n) (countCharsFromLastNewline recognizedStr + 1)
                        )
                    _ -> put (
                            TP t pos,
                            newStr,
                            increaseColumn pos (length recognizedStr)
                            -- Pos line (column + length recognizedStr)
                        )
    where
        countCharsFromLastNewline :: String -> Int
        countCharsFromLastNewline = foldl (\count char -> if char == '\n' then 0 else count + 1) 0
