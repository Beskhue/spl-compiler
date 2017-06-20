module TypeSystem.CheckerTest where

import Data.Either
import Test.Tasty.Hspec as HSpec
import Data.AST
import qualified Lib as Lib
import qualified Parser.SPLParser as SPLParser
import qualified Lexer.Lexer as Lexer
import TypeSystem.Checker as Checker
import qualified Data.Type as Type

parseAndCheckDet file = Checker.checkDet True Checker.emptyCtx . SPLParser.parseDet . Lexer.lexDet file

checkFileRoundTrip file = do
    rawSPL <- Lib.readUTF8File file
    let checker = parseAndCheckDet file in
        checker rawSPL `shouldSatisfy` astEq (checker $ prettyPrint $ checker rawSPL)

expr = SPLParser.parseDet' SPLParser.pExpression . Lexer.lexDet "test"
onlyTypeEmptyTInfExpr = Checker.typeInferenceExpr Checker.tInfExprTyped

spec_Checker :: Spec
spec_Checker = do
    describe "Checker.tInfExpr" $ do
        it "infers that '1;'  is of type TInt" $
            onlyTypeEmptyTInfExpr (expr  "1;") `shouldBe` Right Type.TInt
        it "infers that '1 + 2;' is of type TInt" $
            onlyTypeEmptyTInfExpr (expr  "1 + 2;") `shouldBe` Right Type.TInt
        it "infers that '(1 : []) : [];' is of type [[TInt]]" $
            onlyTypeEmptyTInfExpr (expr "(1 : []) : [];") `shouldBe` Right (Type.TList (Type.TList Type.TInt))
        it "infers that 'False : True : [];' is of type [TBool]" $
            onlyTypeEmptyTInfExpr (expr "True : False : [];") `shouldBe` Right (Type.TList Type.TBool)
        it "infers that '[];' is a list of variable type" $
            onlyTypeEmptyTInfExpr (expr "[];") `shouldSatisfy` (\t ->
                case t of
                    Right (Type.TList (Type.TVar _)) -> True
                    _ -> False)
        it "produces an error for '1 : 'a' : [];'" $
            onlyTypeEmptyTInfExpr (expr "1 : 'a' : [];") `shouldSatisfy` isLeft
        it "produces an error for '(1 : []) : (True : []) : [];'" $
            onlyTypeEmptyTInfExpr (expr "(1 : []) : (True : []) : [];") `shouldSatisfy` isLeft
        it "produces an error for ''a' + 'b';'" $
            onlyTypeEmptyTInfExpr (expr  "'a' + 'b';") `shouldSatisfy` isLeft
    describe "Checker.check" $
        it "Programs survive a parse -> type check -> pretty print -> parse -> type check round trip" $ do
            checkFileRoundTrip "example-programs/fac.spl"
            --parseAndCheckDet "example-programs/tuple_increment.spl"
            checkFileRoundTrip "example-programs/precedence_assoc.spl"
            checkFileRoundTrip "example-programs/last.spl"
            checkFileRoundTrip "example-programs/length.spl"
            checkFileRoundTrip "example-programs/concat.spl"
            checkFileRoundTrip "example-programs/fib.spl"
            checkFileRoundTrip "example-programs/precedence_parentheses.spl"
            checkFileRoundTrip "example-programs/ugly_style.spl"
            checkFileRoundTrip "example-programs/scopes.spl"
            checkFileRoundTrip "example-programs/tct.spl"
            checkFileRoundTrip "example-programs/tcthard.spl"
            checkFileRoundTrip "example-programs/tctpoly.spl"
            checkFileRoundTrip "example-programs/tctpolywithinfun.spl"
            checkFileRoundTrip "example-programs/tctstatements.spl"
            checkFileRoundTrip "example-programs/tctpolyreturn.spl"
            checkFileRoundTrip "example-programs/tctfields.spl"
            checkFileRoundTrip "example-programs/tctannotated.spl"
