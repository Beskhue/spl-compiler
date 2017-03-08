module Parser.SPLParserTest where

import Test.Tasty.Hspec as HSpec
import Data.Pos
import Data.AST
import qualified Lexer.Lexer as Lexer
import qualified Parser.SPLParser as SPLParser

parse = SPLParser.parse . Lexer.lexDet "test"
parseDet = SPLParser.parseDet . Lexer.lexDet "test"

spec_SPLParser :: Spec
spec_SPLParser =
    describe "SPLParser.parse" $ do
        it "parses a variable declaration" $
            parseDet "Int a = 5;" `shouldSatisfy` astEq [
                (DeclV
                    (VarDeclTyped
                        (TypeInt, Pos "" 1 1)
                        (Identifier "a", Pos "" 1 1)
                        (ExprConstant
                            (ConstInt 5, Pos "" 1 1),
                            Pos "" 1 1),
                        Pos "" 1 1), Pos "" 1 1)
            ]
        it "parses left associativity" $
            prettyPrint (parseDet "Int n3 = ((((1 - 2) + 3) - 4) + 5) - 6;") `shouldBe` "Int n3 = 1-2+3-4+5-6;"
        it "programs survive a parse -> pretty print round trip" $ do
            rawSPL <- readFile "example-programs/fac.spl"
            parseDet rawSPL `shouldSatisfy` astEq (parseDet (prettyPrint (parseDet rawSPL)))
            rawSPL <- readFile "example-programs/tuple_increment.spl"
            parseDet rawSPL `shouldSatisfy` astEq (parseDet (prettyPrint (parseDet rawSPL)))
            --rawSPL <- readFile "example-programs/precedence_assoc.spl"
            --parseDet rawSPL `shouldSatisfy` astEq (parseDet (prettyPrint (parseDet rawSPL)))
            rawSPL <- readFile "example-programs/last.spl"
            parseDet rawSPL `shouldSatisfy` astEq (parseDet (prettyPrint (parseDet rawSPL)))
            rawSPL <- readFile "example-programs/length.spl"
            parseDet rawSPL `shouldSatisfy` astEq (parseDet (prettyPrint (parseDet rawSPL)))
            rawSPL <- readFile "example-programs/concat.spl"
            parseDet rawSPL `shouldSatisfy` astEq (parseDet (prettyPrint (parseDet rawSPL)))
            rawSPL <- readFile "example-programs/fib.spl"
            parseDet rawSPL `shouldSatisfy` astEq (parseDet (prettyPrint (parseDet rawSPL)))
