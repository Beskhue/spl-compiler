module Lib where

import System.Environment
import Text.Parsec.Prim
import Data.Token
import qualified Lexer.Lexer as Lexer
import qualified Parser.SPLParser as SPLParser

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
    rawSPL <- readFile filePath
    putStrLn $ "Read '" ++ filePath ++ "'."
    case Lexer.lex filePath rawSPL of
        Left e    -> putStrLn $ show e
        Right ts -> SPLParser.parseTest SPLParser.pSPL ts

parseTest  :: Show a => Parsec [TokenP] () a -> String -> IO ()
parseTest p s =
    case Lexer.lex "test" s of
        Left e    -> putStrLn $ show e
        Right ts -> SPLParser.parseTest p ts
