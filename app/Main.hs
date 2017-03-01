module Main where

import Lib
import System.Environment
import Data.NFA as NFA
import Data.DFA as DFA
import qualified Text.Regex.Thompson as Regex
import qualified Lexer.Lexer as Lexer

main :: IO ()
main = do
    args <- getArgs
    if length args > 0
        then do
            let filePath = head args
            rawSPL <- readFile filePath
            putStrLn $ "Read '" ++ filePath ++ "'."
            putStrLn rawSPL
            -- rawSPL <- readFile filePath
            -- putStrLn rawSPL
        else putStrLn "Please provide a .spl program"

