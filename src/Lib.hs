module Lib where

import System.Environment
import System.IO
import Text.Parsec.Prim
import Data.Token
import Data.AST as AST
import qualified Lexer.Lexer as Lexer
import qualified Parser.SPLParser as SPLParser
import qualified TypeSystem.Checker as Checker
import qualified CodeGenerator.SSM as SSM
import qualified Data.Map as Map

getFilePath :: IO String
getFilePath = do
    args <- getArgs
    if length args > 0
        then return $ head args
        else do
            putStr "Please provide a .spl program: "
            getLine

compile :: IO ()
compile = do
    filePath <- getFilePath
    rawSPL <- readUTF8File filePath
    putStrLn $ "Read '" ++ filePath ++ "'."
    case Lexer.lex filePath rawSPL of
        Left e    -> putStrLn $ show e
        Right ts -> case SPLParser.parse ts of
            Left e -> putStrLn $ show e
            Right ast -> case Checker.check ast of
                Left e -> putStrLn $ show e
                --Right (b, annotation) -> putStrLn $ AST.prettyPrint b ++ "\n\n" ++ show b ++ "\n\n" ++ show (Map.assocs annotation)
                Right (b, annotation) -> case SSM.gen annotation b of
                    Left e -> putStrLn $ show e
                    Right ssm -> putStrLn $ SSM.display ssm

prettyPrint :: IO()
prettyPrint = do
    filePath <- getFilePath
    rawSPL <- readUTF8File filePath
    putStrLn $ "Read '" ++ filePath ++ "'."
    case Lexer.lex filePath rawSPL of
        Left e    -> putStrLn $ show e
        Right ts -> case SPLParser.parse ts of
            Left e -> putStrLn $ show e
            Right ast -> putStrLn $ AST.prettyPrint ast

parseTest  :: Show a => Parsec [TokenP] () a -> String -> IO ()
parseTest p s =
    case Lexer.lex "test" s of
        Left e    -> putStrLn $ show e
        Right ts -> SPLParser.parseTest p ts

-- |Read a file in UTF8 encoding
readUTF8File :: String -> IO String
readUTF8File filePath = do
    inputHandle <- openFile filePath ReadMode
    hSetEncoding inputHandle utf8
    hGetContents inputHandle
