module Lib where

import System.Environment
import System.IO
import System.IO.Error
import Text.Parsec.Prim
import Data.Token
import qualified Data.AST as AST
import qualified Data.Type as Type
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
    a <- checkWithIncludes ast
    let spl = concat a
    ssm <- eitherToRight $ SSM.gen spl
    putStrLn $ SSM.display ssm
    writeFile "output/program.ssm" (SSM.display ssm)
    putStrLn "Compiled to 'output/program.ssm'."

prettyPrint :: IO ()
prettyPrint = do
    ast <- askAndParse
    a <- checkWithIncludes ast
    let spl = head a
    putStrLn $ AST.prettyPrint spl

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

-- |Read and parse
readAndParse :: String -> IO AST.SPL
readAndParse filePath = do
    rawSPL <- readUTF8File filePath
    putStrLn $ "Read '" ++ filePath ++ "'."
    Lib.parse (rawSPL, filePath)

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

--------------------------------------------------------------------------------
-- Linking

typedASTToCtx :: AST.SPL -> Map.Map String Type.Scheme
typedASTToCtx [] = Map.empty
typedASTToCtx (decl:decls) =
    let schemes = typedASTToCtx decls in
        case decl of
            (AST.DeclI _, _) -> schemes
            (AST.DeclC (AST.ClassDecl classId vs fs, _), _) ->
                let schemes' = foldr (\decl schemes ->
                        case decl of
                            (AST.VarDeclTyped t i _, _) ->
                                Map.insert (Checker.classIDName classId ++ "." ++ Checker.idName i) (Checker.generalize Checker.emptyScopedCtx (Checker.rTranslateType t)) schemes
                            (AST.VarDeclTypedUnitialized t i, _) ->
                                Map.insert (Checker.classIDName classId ++ "." ++ Checker.idName i) (Checker.generalize Checker.emptyScopedCtx (Checker.rTranslateType t)) schemes
                        ) schemes vs in
                foldr (\(AST.FunDeclTyped i _ t _, _) schemes ->
                        Map.insert (Checker.classIDName classId ++ "." ++ Checker.idName i) (Checker.generalize Checker.emptyScopedCtx (Checker.rTranslateType t)) schemes
                    ) schemes' fs
            (AST.DeclV (AST.VarDeclTyped t i _, _), _) ->
                Map.insert (Checker.idName i) (Checker.generalize Checker.emptyScopedCtx (Checker.rTranslateType t)) schemes
            (AST.DeclV (AST.VarDeclTypedUnitialized t i, _), _) ->
                Map.insert (Checker.idName i) (Checker.generalize Checker.emptyScopedCtx (Checker.rTranslateType t)) schemes
            (AST.DeclF (AST.FunDeclTyped i _ t _, _), _) ->
                Map.insert (Checker.idName i) (Checker.generalize Checker.emptyScopedCtx (Checker.rTranslateType t)) schemes

checkWithIncludes :: AST.SPL -> IO [AST.SPL]
checkWithIncludes spl = do
    (r, _, _) <- checkWithIncludes' spl [] [] [] Map.empty
    return r
    where
        checkWithIncludes' ::
            AST.SPL ->
            AST.SPL ->
            [AST.SPL] ->
            [String] ->
            Map.Map String Type.Scheme ->
            IO ([AST.SPL], [String], Map.Map String Type.Scheme)
        checkWithIncludes' (d@(AST.DeclI (AST.IncludeDecl s, _), _):decls) includeDecls spls included accum =
            if s `elem` included
                then checkWithIncludes' decls (d:includeDecls) spls included accum
                else do
                    spl <- Lib.readAndParse s
                    (r, included', schemes) <- checkWithIncludes' spl [] [] (s : included) accum
                    checkWithIncludes' decls (d:includeDecls) (r ++ spls) (s : included' ++ included) (Map.union accum schemes)
        checkWithIncludes' decls includeDecls spls included accum = do
            spl <- Lib.eitherToRight $ Checker.check False (Checker.TypeCtx accum) decls
            return ((includeDecls ++ spl) : spls, included, Map.union accum (typedASTToCtx spl))
