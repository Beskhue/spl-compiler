module Main where

import Lib
import System.Environment
import Data.NFA as NFA
import qualified Text.Regex.Thompson as Regex
import qualified Lexer.Lexer as Lexer
import qualified Parser.Parser as Parser

main :: IO ()
main = do
    filePath <- getFilePath
    rawSPL <- readFile filePath
    putStrLn $ "Read '" ++ filePath ++ "'."
    putStrLn rawSPL

getFilePath :: IO String
getFilePath = do
    args <- getArgs
    if length args > 0
        then return $ head args
        else do
            putStr "Please provide a .spl program: "
            getLine