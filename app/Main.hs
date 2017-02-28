module Main where

import Lib
import System.Environment
import Data.NFA as NFA
import Data.DFA as DFA
import Text.Regex.Thompson as Regex

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

