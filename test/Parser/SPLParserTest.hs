module Parser.SPLParserTest where

import Test.Tasty.Hspec as HSpec
import Data.Pos
import Data.AST
import qualified Lib as Lib
import qualified Lexer.Lexer as Lexer
import qualified Parser.SPLParser as SPLParser

parse = SPLParser.parse . Lexer.lexDet "test"
parseDet file = SPLParser.parseDet . Lexer.lexDet file
parseDetTest = parseDet "test"

checkFileRoundTrip file = do
    rawSPL <- Lib.readUTF8File file
    let parser = parseDet file in
        parser rawSPL `shouldSatisfy` astEq (parser $ prettyPrint $ parser rawSPL)

spec_SPLParser :: Spec
spec_SPLParser =
    describe "SPLParser.parse" $ do
        it "parses a variable declaration" $
            parseDetTest "Int a = 5;" `shouldSatisfy` astEq [
                (DeclV
                    (VarDeclTyped
                        (TypeInt, emptyMeta)
                        (Identifier "a", emptyMeta)
                        (ExprConstant
                            (ConstInt 5, emptyMeta),
                            emptyMeta),
                        emptyMeta), emptyMeta)
            ]
        it "parses left associativity" $
            prettyPrint (parseDetTest "Int n3 = ((((1 - 2) + 3) - 4) + 5) - 6;") `shouldBe` "Int n3 = 1 - 2 + 3 - 4 + 5 - 6;"
        it "programs survive a parse -> pretty print round trip" $ do
            checkFileRoundTrip "example-programs/fac.spl"
            checkFileRoundTrip "example-programs/tuple_increment.spl"
            checkFileRoundTrip "example-programs/precedence_assoc.spl"
            checkFileRoundTrip "example-programs/last.spl"
            checkFileRoundTrip "example-programs/length.spl"
            checkFileRoundTrip "example-programs/concat.spl"
            checkFileRoundTrip "example-programs/fib.spl"
            checkFileRoundTrip "example-programs/precedence_parentheses.spl"
            checkFileRoundTrip "example-programs/ugly_style.spl"
            checkFileRoundTrip "example-programs/scopes.spl"
            checkFileRoundTrip "example-programs/tctstatements.spl"
            -- Todo: fix UTF8 output of prettyPrint
            --checkFileRoundTrip "example-programs/unicode.spl"
            --checkFileRoundTrip "example-programs/performance_test_parentheses.spl"
