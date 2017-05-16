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
    a <- checkWithIncludes ast
    let spl = concatMap fst a
    let annotation = foldr (Map.union . snd) Map.empty a
    ssm <- eitherToRight $ SSM.gen annotation spl
    putStrLn $ SSM.display ssm

prettyPrint :: IO ()
prettyPrint = do
    ast <- askAndParse
    a <- checkWithIncludes ast
    let spl = fst $ head a
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

typedASTToCtx :: AST.SPL -> Map.Map String Checker.Scheme
typedASTToCtx [] = Map.empty
typedASTToCtx (decl:decls) =
    let schemes = typedASTToCtx decls in
        case decl of
            (AST.DeclI _, _) -> schemes
            (AST.DeclV (AST.VarDeclTyped t i _, _), _) ->
                Map.insert (Checker.idName i) (Checker.generalize Checker.emptyScopedCtx (Checker.rTranslateType t)) schemes
            (AST.DeclF (AST.FunDeclTyped i _ t _, _), _) ->
                Map.insert (Checker.idName i) (Checker.generalize Checker.emptyScopedCtx (Checker.rTranslateFunType t)) schemes

checkWithIncludes :: AST.SPL -> IO [(AST.SPL, Checker.ASTAnnotation)]
checkWithIncludes spl = do
    r <- checkWithIncludes' spl [] [] [] Map.empty
    return $ fst r
    where
        checkWithIncludes' ::
            AST.SPL ->
            AST.SPL ->
            [(AST.SPL, Checker.ASTAnnotation)] ->
            [String] ->
            Map.Map String Checker.Scheme ->
            IO ([(AST.SPL, Checker.ASTAnnotation)], Map.Map String Checker.Scheme)
        checkWithIncludes' (d@(AST.DeclI (AST.IncludeDecl s, _), _):decls) includeDecls spls included accum =
            if s `elem` included
                then checkWithIncludes' decls (d:includeDecls) spls included accum
                else do
                    spl <- Lib.readAndParse s
                    (r, schemes) <- checkWithIncludes' spl [] spls (s : included) accum
                    checkWithIncludes' decls (d:includeDecls) (r ++ spls) (s : included) (Map.union accum schemes)
        checkWithIncludes' decls includeDecls spls included accum = do
            (spl, annotation) <- Lib.eitherToRight $ Checker.check False (Checker.TypeCtx accum) decls
            return ((includeDecls ++ spl, annotation) : spls, Map.union accum (typedASTToCtx spl))
