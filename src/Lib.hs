module Lib where

import Text.Parsec.Prim
import Data.Token
import qualified Lexer.Lexer as Lexer
import qualified Parser.SPLParser as SPLParser

compile :: String -> IO ()
compile str = putStrLn "someFunc"

parseTest  :: Show a => Parsec [TokenP] () a -> String -> IO ()
parseTest p s =
    case Lexer.lex "test" s of
        Left e    -> putStrLn $ show e
        Right ts -> SPLParser.parseTest p ts
