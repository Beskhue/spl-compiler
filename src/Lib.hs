module Lib where

import System.Environment
import System.IO
import System.IO.Error
import Text.Parsec.Prim
import Data.Token
import Data.AST as AST
import qualified Lexer.Lexer as Lexer
import qualified Parser.SPLParser as SPLParser
import qualified TypeSystem.Checker as Checker
import qualified CodeGenerator.SSM as SSM
import qualified Data.Map as Map

type FilePath = String
type RawSPL = String

compile :: IO ()
compile = do
    ast <- askAndParse
    (ast', annotation) <- eitherToRight $ Checker.check False ast
    ssm <- eitherToRight $ SSM.gen annotation ast'
    putStrLn $ SSM.display ssm

prettyPrint :: IO ()
prettyPrint = do
    ast <- askAndParse
    (ast', _) <- eitherToRight $ Checker.check False ast
    putStrLn $ AST.prettyPrint ast'

parse :: (RawSPL, Lib.FilePath) -> IO AST.SPL
parse (rawSPL, filePath) = do
    ts <- eitherToRight $ Lexer.lex filePath rawSPL
    eitherToRight $ SPLParser.parse ts

-- |Ask the user for a file, read it, and parse it
askAndParse :: IO AST.SPL
askAndParse = do
    f <- askForFile
    Lib.parse f

-- |Ask the user for a file and read it
askForFile :: IO (RawSPL, Lib.FilePath)
askForFile = do
    filePath <- getFilePath
    rawSPL <- readUTF8File filePath
    putStrLn $ "Read '" ++ filePath ++ "'."
    return (rawSPL, filePath)

-- |Ask the user for a file path
getFilePath :: IO String
getFilePath = do
    args <- getArgs
    if not $ null args
        then return $ head args
        else do
            putStr "Please provide a .spl program: "
            getLine

-- |Read a file in UTF8 encoding
readUTF8File :: String -> IO String
readUTF8File filePath = do
    inputHandle <- openFile filePath ReadMode
    hSetEncoding inputHandle utf8
    hGetContents inputHandle

-- |Return the right part of an either, or display the left part as an error
eitherToRight :: Show a => Either a b -> IO b
eitherToRight (Left a) = ioError $ userError $ show a
eitherToRight (Right b) = return b
