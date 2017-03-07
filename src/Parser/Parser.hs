module Parser.Parser where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Except
import Data.Token
import Data.Pos
import qualified Data.AST as AST

type AST = (AST', Pos)
data AST' = AChar Char
         | AExpr AST AST
           deriving (Show)

data ParseError = UnexpectedToken Token Token
                | InvalidSyntax Token
                | SetMustContainItem
                | GenericError
                  deriving (Show, Eq)
type ParseErrorP = (ParseError, Pos)

{-
instance Show ParseError where
    show (UnexpectedToken t t') = "Unexpected token " ++ show t' ++ ". Expected: " ++ show t ++ "."
    show SetMustContainItem     = "A character set must contain at least one item."
-}

{-|
Define parse with a state monad stacked with an exception monad
See http://hackage.haskell.org/package/transformers-0.5.4.0/docs/Control-Monad-Trans-Class.html
-}
type ParserT = StateT (TokenP, [TokenP]) (Except ParseErrorP)

{-
newtype Parser a = Parser ([TokenP] -> [(a, [TokenP])])

parse' (Parser p) = p

instance Functor Parser where
    fmap = liftM

instance Applicative Parser where
    pure  = return
    (<*>) = ap

instance Monad Parser where
    return a = Parser (\ts -> [(a, ts)])
    p >>= f  = Parser (\ts -> concat [parse' (f a) ts' | (a, ts') <- parse' p ts])

type ParserE = ExceptT (ParseErrorP) Parser
-}

parseTest :: (Show a) => ParserT a -> [TokenP] -> IO ()
parseTest p input
    = case parse p input of
        Left err -> do putStr "parse error at "
                       print err
        Right x -> print x

getTokenP :: ParserT TokenP
getTokenP = do
    (t, _) <- get
    return t

getToken :: ParserT Token
getToken = do
    TP t _ <- getTokenP
    return t

getPos :: ParserT Pos
getPos = do
    TP _ pos <- getTokenP
    return pos

-- |Throw a parse error
throwParseError :: ParseError -> ParserT ()
throwParseError pError = do
    p <- getPos
    lift $ throwE (pError, p)

advance :: ParserT ()
advance = do
    (_, t:ts) <- get
    put (t, ts)

eat :: Token -> ParserT ()
eat t = do
    t' <- getToken
    p <- getPos
    if t == t'
        then advance
        else throwParseError (UnexpectedToken t t')

(+++) :: ParserT [a] -> ParserT [a] -> ParserT [a]
p +++ q = do
    p' <- p
    q' <- q
    case p' ++ q' of
        x:xs -> return [x]
        [] -> return []



{-
many :: ParserT ASTP -> ParserT [ASTP]
many p = do
    many1 p +++ return []


pSpl :: ParserT [ASTP]
pSpl =
-}

try :: ParserT AST -> ParserT AST
try p = do
    state <- get
    lift (catchE  (evalStateT p state) (\e -> return undefined))


pChar :: ParserT AST
pChar = do
    (TP t p) <- getTokenP
    case t of
        TConstant (CChar c) -> eat t >> return (AChar c, p)
        _ -> throwParseError (InvalidSyntax t) >> return undefined

-- |Parse a list of tokens into an abstract syntax tree
parse :: ParserT a -> [TokenP] -> Either ParseErrorP a
parse p tokens = runExcept $ evalStateT (advance >> p) (undefined, tokens)
