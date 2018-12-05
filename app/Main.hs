module Main where

import Lib
import System.Environment
-- import Data.NFA as NFA
-- import qualified Text.Regex.Thompson as Regex
import qualified Lexer.Lexer as Lexer
import qualified Parser.SPLParser as SPLParser
import qualified TypeSystem.Checker as Checker

main :: IO ()
main = do
    putStrLn "splc 0.1.0.0"
    compile
