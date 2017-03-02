{-|
Module: Thompson
Description: A regex engine
Copyright: (c) Thomas Churchman, 2017
License: MIT
Maintainer: thomas@kepow.org
Stability: experimental

A regex engine based on ThompsonTest's construction

See:
Cox, R. (2007). Regular expression matching can be simple and fast (but is slow in java, perl, php, python,
ruby, ...).
http://www.diku.dk/hjemmesider/ansatte/henglein/papers/cox2007.pdf

ThompsonTest, S. (2000). Regular expressions and automata using Haskell.
http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.650.7393&rep=rep1&type=pdf

TODO: group matching, https://swtch.com/~rsc/regexp/regexp2.html

-}

module Text.Regex.Thompson where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Except
import Data.Functor.Identity
import Data.NFA as NFA

data Regex = Lambda
           | EOS
           | All
           | Literal Char
           | Union Regex Regex
           | Concat Regex Regex
           | Star Regex
           | Option Regex
           | Group Regex
           | Set SetItems
           | NegSet SetItems
             deriving (Show, Eq)

data SetItem = SetLiteral Char
             | SetRange Char Char
               deriving (Show, Eq)

type SetItems = [SetItem]

-- |Define a function to print regexes in a familiar format
printRegex :: Regex -> String
printRegex Lambda = "λ"
printRegex All = "."
printRegex (Literal c) = [c]
printRegex (Union r1 r2) = "(" ++ printRegex r1 ++ ")|(" ++ printRegex r2 ++ ")"
printRegex (Concat r1 r2) = printRegex r1 ++ printRegex r2
printRegex (Star r) = "(" ++ printRegex r ++ ")*"
printRegex (Option r) = "(" ++ printRegex r ++ ")?"
printRegex (Group r) = "(" ++ printRegex r ++ ")"
printRegex (Set setItems) = "[...]"
printRegex (NegSet setItems) = "[^...]"

{-
instance Show Regex where
    show = printRegex
-}

data Token = TUnion
             | TAll
             | TStar
             | TPlus
             | TOption
             | TGroupOpen
             | TGroupClose
             | TSetOpen
             | TSetClose
             | TNegSet
             | TRangeDelimiter
             | TEscape
             | TEOS
             | TEOF
             | TChar Char
               deriving (Show, Eq)

-- |Tokenize a character
token :: Char -> Token
token '|'   = TUnion
token '.'   = TAll
token '*'   = TStar
token '+'   = TPlus
token '?'   = TOption
token '('   = TGroupOpen
token ')'   = TGroupClose
token '['   = TSetOpen
token ']'   = TSetClose
token '^'   = TNegSet
token '-'   = TRangeDelimiter
token '\\'  = TEscape -- \
token '$'   = TEOS
token c     = TChar c

tokenToChar :: Token -> Char
tokenToChar TUnion = '|'
tokenToChar TAll = '.'
tokenToChar TStar = '*'
tokenToChar TPlus = '+'
tokenToChar TOption = '?'
tokenToChar TGroupOpen = '('
tokenToChar TGroupClose = ')'
tokenToChar TSetOpen = '['
tokenToChar TSetClose = ']'
tokenToChar TNegSet = '^'
tokenToChar TRangeDelimiter = '-'
tokenToChar TEscape = '\\'
tokenToChar TEOS = '$'
tokenToChar TEOF = '\0'
tokenToChar (TChar c) = c

-- |Tokenize a string
tokenize :: String -> [Token]
tokenize s = cleanupEscapes [token c | c <- s] ++ [TEOF]
    where
        -- |Cleanup escapes in a regex (e.g. [.., (TEscape, '\\'), (TStar, '*'), ..] becomes [.., (TChar, '*'), ..])
        cleanupEscapes :: [Token] -> [Token]
        cleanupEscapes (TEscape : t : ts) = TChar (tokenToChar t) : cleanupEscapes ts
        cleanupEscapes (t : ts) = t : cleanupEscapes ts
        cleanupEscapes [] = []


{-| Regular expression grammar:
    Adapted from http://www.cs.sfu.ca/~cameron/Teaching/384/99-3/regexp-plg.html (accessed 2017-02-24)

    <regex>             ::= <term> '|' <regex>
                            | <term>
    <term>              ::= <basic-regex> <term> | <basic-regex>
    <basic-regex>       ::= <base> '*' | <base> '+' | <base> '?' | <base> | '$'
    <base>              ::= <group> | <set> | '.' | <char>
    <group>             ::= '(' <regex> ')'
    <set>               ::= '[' <set-items> ']'
    <pos-set>           ::= <set-items>
    <neg-set>           ::= '^' <set-items>
    <set-items>         ::= <set-item> | <set-item> <set-items>
    <set-item>          ::= <char> '-' <char> | <char>
-}



data ParseError = UnexpectedToken Token Token
                | SetMustContainItem
                  deriving (Eq)
type ParseErrorN = (ParseError, Int)

instance Show ParseError where
    show (UnexpectedToken t t') = "Unexpected token " ++ show t' ++ ". Expected: " ++ show t ++ "."
    show SetMustContainItem     = "A character set must contain at least one item."

{-|
Define parse with a state monad stacked with an exception monad
See http://hackage.haskell.org/package/transformers-0.5.4.0/docs/Control-Monad-Trans-Class.html

A state contains (Current Token, Remaining Tokens, Current position being parsed)
-}
type ParseT = StateT (Token, [Token], Int) (Except ParseErrorN)

-- |Get the current token from the state
getToken :: ParseT Token
getToken = do
    (t, _, _) <- get
    return t

-- |Get the current position being parsed from the state
getN :: ParseT Int
getN = do
    (_, _, n) <- get
    return n

-- |Attempt to parse a string, or fail with an error message
parse :: String -> Regex
parse str =
    case p of
        Left pError -> error (parseErrorMsg str pError)
        Right r -> r
    where
        p = parseRegex str

-- |Create an error message given a parse error
parseErrorMsg :: String -> ParseErrorN -> String
parseErrorMsg str (pError, n) =
       intro
    ++ "\n"
    ++ marker
    ++ "\nError: " ++ show pError
    ++ "\nAt position: " ++ show n
    where intro = "Failed to parse:\n/" ++ str ++ "/"
          marker = concat (replicate n " ") ++ "^"

-- |Throw a parse error
throwParseError :: ParseError -> ParseT ()
throwParseError pError = do
    n <- getN
    lift $ throwE (pError, n)

-- |Parse a regex string to either a parse error or a regex
parseRegex :: String -> Either ParseErrorN Regex
parseRegex str = runExcept $ evalStateT (advance >> pRegex) (undefined, tokenize str, 0)

-- |Advance the state to the next state (i.e., set the next token as the current token, and increment the position counter)
advance :: ParseT ()
advance = do
    (_, x:xs, n) <- get
    (put (x, xs, n + 1))

-- |"Eat" a token from the input: tests if the token is expected, and if so advances the state.
eat :: Token -> ParseT ()
eat t = do
    t' <- getToken
    n <- getN
    if t == t'
        then advance
        else throwParseError (UnexpectedToken t t')

pRegex :: ParseT Regex -- <regex> ->
pRegex = do
    term <- pTerm
    token <- getToken
    case token of
        TUnion -> eat TUnion >> liftM (Union term) pRegex -- <term> '|' <regex>
        _ -> return term -- <term>


pTerm :: ParseT Regex -- <term> ->
pTerm = do
    basicRegex <- pBasicRegex
    token <- getToken
    case token of
        TEOF -> return basicRegex -- <basic-regex>
        TUnion -> return basicRegex -- <basic-regex>
        TGroupClose -> return basicRegex -- <base-regex>
        _ -> liftM (Concat basicRegex) pTerm -- <basic-regex> <term>

pBasicRegex :: ParseT Regex -- <basic-regex> ->
pBasicRegex = do
    token <- getToken
    case token of
        TEOS -> eat TEOS >> return EOS
        _ -> do
            base <- pBase
            token <- getToken
            case token of
                TStar -> eat TStar >> return (Star base) -- <base> '*'
                TPlus -> eat TPlus >> return (Concat base (Star base)) -- <base> '+'
                TOption -> eat TOption >> return (Option base) -- <base> '?'
                _ -> return base -- <base>

pBase :: ParseT Regex -- <base> ->
pBase = do
    token <- getToken
    case token of
        TGroupOpen -> pGroup
        TSetOpen -> pSet
        TAll -> do
            eat TAll
            return All
        _ -> pLiteral

pGroup :: ParseT Regex -- <group> -> '(' <regex> ')'
pGroup = do
    eat TGroupOpen
    regex <- pRegex
    eat TGroupClose
    return $ Group regex

pSet :: ParseT Regex -- <set> -> '[' <set-items> ']'
pSet = do
    eat TSetOpen
    token <- getToken
    case token of
        TNegSet -> do
            eat TNegSet
            setItems <- pSetItems
            eat TSetClose
            return $ NegSet setItems
        _ -> do
            setItems <- pSetItems
            eat TSetClose
            return $ Set setItems

pSetItems :: ParseT SetItems -- <set-items> ->
pSetItems = pSetItems' 0
    where
        pSetItems' :: Int -> ParseT SetItems
        pSetItems' n = do
            token <- getToken
            case token of
                TSetClose -> if n == 0 -- Nothing
                    then (throwParseError SetMustContainItem) >> return []
                    else return []
                _ -> do
                    setItem <- pSetItem -- <set-item> <set-items>
                    liftM (setItem :) (pSetItems' (n+1))

pSetItem :: ParseT SetItem -- <set-item> ->
pSetItem = do
    char <- pChar
    token <- getToken
    case token of
        TRangeDelimiter -> do -- <char> '-' <char>
            eat TRangeDelimiter
            char2 <- pChar
            return $ SetRange char char2
        _ -> return $ SetLiteral char -- <char>

pLiteral :: ParseT Regex -- <char>
pLiteral = do
    token <- getToken
    case token of
        TEOF -> return Lambda
        TUnion -> return Lambda
        TGroupClose -> return Lambda
        _ -> liftM Literal pChar

pChar :: ParseT Char -- <char>
pChar = do
    token <- getToken
    let c = tokenToChar token
    eat (TChar c)
    return c

-- |A regex is compiled to an NFA with integer states
type CompiledRegex = NFA.NFA Int

-- |Compile a regex string to a regex
compile :: String -> CompiledRegex
compile str = addEOSTransitions $ normalizeNFA (build (parse str))

-- |Normalize a regex NFA
normalizeNFA :: NFA.NFA Int -> NFA.NFA Int
normalizeNFA (NFA.NFA states trans init accs) = nfaIncrementIndices (NFA.NFA states trans init accs) (- (head states))

addEOSTransitions :: NFA.NFA Int -> NFA.NFA Int
addEOSTransitions (NFA.NFA states trans init accs) = NFA.NFA states newTrans init accs
    where
        newTrans = trans ++ [NFA.EOSTransition s s | s <- accs]

-- |Compile a regex to an NFA
build :: Regex -> NFA.NFA Int
build Lambda = NFA.NFA
    [0]
    []
    0
    [0]
build EOS = NFA.NFA
    [0, 1]
    [NFA.EOSTransition 0 1]
    0
    [1]
build (Literal c) = NFA.NFA
    [0 .. 1]
    [NFA.Transition 0 c 1]
    0
    [1]
build (Union r1 r2) = buildUnion (build r1) (build r2)
build (Concat r1 r2) = buildConcat (build r1) (build r2)
build All = NFA.NFA
    [0, 1]
    [NFA.ConditionalTransition 0 (\c -> True) 1]
    0
    [1]
build (Star r) = buildStar (build r)
build (Option r) = buildOption (build r)
build (Group r) = buildGroup (build r)
build (Set setItems) = NFA.NFA
    [0, 1]
    [NFA.ConditionalTransition 0 condition 1]
    0
    [1]
        where
            condition :: Char -> Bool
            condition a = elem a $ concat [(
                    case setItem of
                        SetLiteral c -> [c]
                        SetRange c1 c2 -> [c1 .. c2]
                ) | setItem <- setItems]
build (NegSet setItems) = NFA.NFA
      [0, 1]
      [NFA.ConditionalTransition 0 condition 1]
      0
      [1]
          where
              condition :: Char -> Bool
              condition a = not $ elem a $ concat [(
                      case setItem of
                          SetLiteral c -> [c]
                          SetRange c1 c2 -> [c1 .. c2]
                  ) | setItem <- setItems]
{-
build (Set setItems) = NFA.NFA
    [0, 1]
    (concat [setItemToTransition setItem | setItem <- setItems])
    0
    [1]
        where
        setItemToTransition :: SetItem -> [NFA.Transition Int]
        setItemToTransition (SetLiteral c) = [NFA.Transition 0 c 1]
        setItemToTransition (SetRange c1 c2) = [NFA.Transition 0 c 1 | c <- [c1 .. c2]]
-}

buildUnion :: NFA.NFA Int -> NFA.NFA Int -> NFA.NFA Int
buildUnion (NFA.NFA states1 trans1 init1 accs1) (NFA.NFA states2 trans2 init2 accs2) =
    let
        f = head states1 - 1
        (NFA.NFA states2' trans2' init2' accs2') = nfaIncrementIndices
                                                       (NFA.NFA states2 trans2 init2 accs2)
                                                       ((last states1) - (head states2) + 1)
        l = last states2'
        newStates = [f] ++ states1 ++ states2' ++ [l]
        newTrans =
             [NFA.EmptyTransition f init1, NFA.EmptyTransition f init2']
          ++ [NFA.EmptyTransition acc l | acc <- accs1]
          ++ [NFA.EmptyTransition acc l | acc <- accs2']
          ++ trans1 ++ trans2'
    in
        NFA.NFA
            newStates
            newTrans
            f
            [l]


buildConcat :: NFA.NFA Int -> NFA.NFA Int -> NFA.NFA Int
buildConcat (NFA.NFA states1 trans1 init1 accs1) (NFA.NFA states2 trans2 init2 accs2) =
    let
        f = head states1 - 1
        (NFA.NFA states2' trans2' init2' accs2') = nfaIncrementIndices
                                                       (NFA.NFA states2 trans2 init2 accs2)
                                                       ((last states1) - (head states2) + 1)
        l = last states2' + 1
        newStates = [f] ++ states1 ++ states2' ++ [l]
        newTrans =
             [NFA.EmptyTransition f init1]
          ++ [NFA.EmptyTransition acc init2' | acc <- accs1]
          ++ [NFA.EmptyTransition acc l | acc <- accs2']
          ++ trans1 ++ trans2'
    in
        NFA.NFA
        newStates
        newTrans
        f
        [l]

buildStar :: NFA.NFA Int -> NFA.NFA Int
buildStar (NFA.NFA states trans init accs) = NFA.NFA
    newStates
    newTrans
    f
    [l]
        where f = head states - 1
              l = last states + 1
              newStates = [f] ++ states ++ [l]
              newTrans =
                   [NFA.EmptyTransition f l, NFA.EmptyTransition l f, NFA.EmptyTransition f init]
                ++ [NFA.EmptyTransition acc l | acc <- accs]
                ++ trans

buildOption :: NFA.NFA Int -> NFA.NFA Int
buildOption (NFA.NFA states trans init accs) = NFA.NFA
    newStates
    newTrans
    f
    [l]
        where f = head states - 1
              l = last states + 1
              newStates = [f] ++ states ++ [l]
              newTrans =
                   [NFA.EmptyTransition f l, NFA.EmptyTransition f init]
                ++ [NFA.EmptyTransition acc l | acc <- accs]
                ++ trans

buildGroup :: NFA.NFA Int -> NFA.NFA Int
buildGroup nfa = nfa

nfaIncrementIndices :: NFA.NFA Int -> Int -> NFA.NFA Int
nfaIncrementIndices (NFA.NFA states trans init accs) inc = NFA.NFA
    [st + inc | st <- states]
    [case tran of
        NFA.Transition i1 c i2 -> NFA.Transition (i1 + inc) c (i2 + inc)
        NFA.ConditionalTransition i1 c i2 -> NFA.ConditionalTransition (i1 + inc) c (i2 + inc)
        NFA.EmptyTransition i1 i2 -> NFA.EmptyTransition (i1 + inc) (i2 + inc)
        NFA.EOSTransition i1 i2 -> NFA.EOSTransition (i1 + inc) (i2 + inc)
            | tran <- trans]
    (init + inc)
    [acc + inc | acc <- accs]

match :: CompiledRegex -> String -> Bool
match r s = False

-- |Test whether a (compiled) regex recognizes a string
evaluate :: CompiledRegex -> String -> Bool
evaluate = NFA.evaluate

-- |Finds all substrings (expanding to the right) that are recognized by the (compiled) regex
matches :: CompiledRegex -> String -> [String]
matches = NFA.matches

-- |Finds the longest substring recognized by the (compiled) regex (the substrings expand to the right, i.e. they match "^regex")
longestMatch :: CompiledRegex -> String -> Maybe String
longestMatch = NFA.longestMatch

-- |Finds the shortest substring recognized by the (compiled) regex (the substrings expand to the right, i.e. they match "^regex")
shortestMatch :: CompiledRegex -> String -> Maybe String
shortestMatch = NFA.shortestMatch
