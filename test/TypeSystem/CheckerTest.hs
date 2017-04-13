module TypeSystem.CheckerTest where

import Data.Either
import Test.Tasty.Hspec as HSpec
import Data.AST
import qualified Lib as Lib
import qualified Parser.SPLParser as SPLParser
import qualified Lexer.Lexer as Lexer
import TypeSystem.Checker as Checker

parseAndCheckDet = Checker.checkDet . SPLParser.parseDet . Lexer.lexDet "test"

expr = SPLParser.parseDet' SPLParser.pExpression . Lexer.lexDet "test"
emptyTInfExpr = Checker.typeInference Checker.tInfExpr Checker.emptyScopedCtx
onlyTypeEmptyTInfExpr = Checker.onlyType . emptyTInfExpr

spec_Checker :: Spec
spec_Checker = do
    describe "Checker.tInfExpr" $ do
        it "infers that '1;'  is of type TInt" $
            onlyTypeEmptyTInfExpr (expr  "1;") `shouldBe` Right Checker.TInt
        it "infers that '1 + 2;' is of type TInt" $
            onlyTypeEmptyTInfExpr (expr  "1 + 2;") `shouldBe` Right Checker.TInt
        it "infers that '(1 : []) : [];' is of type [[TInt]]" $
            onlyTypeEmptyTInfExpr (expr "(1 : []) : [];") `shouldBe` Right (Checker.TList (Checker.TList Checker.TInt))
        it "infers that 'False : True : [];' is of type [TBool]" $
            onlyTypeEmptyTInfExpr (expr "True : False : [];") `shouldBe` Right (Checker.TList TBool)
        it "infers that '[];' is a list of variable type" $
            onlyTypeEmptyTInfExpr (expr "[];") `shouldSatisfy` (\t ->
                case t of
                    Right (Checker.TList (Checker.TVar _)) -> True
                    _ -> False)
        it "produces an error for '1 : 'a' : [];'" $
            onlyTypeEmptyTInfExpr (expr "1 : 'a' : [];") `shouldSatisfy` isLeft
        it "produces an error for '(1 : []) : (True : []) : [];'" $
            onlyTypeEmptyTInfExpr (expr "(1 : []) : (True : []) : [];") `shouldSatisfy` isLeft
        it "produces an error for ''a' + 'b';'" $
            onlyTypeEmptyTInfExpr (expr  "'a' + 'b';") `shouldSatisfy` isLeft
    describe "Checker.check" $
        it "Programs survive a parse -> type check -> pretty print -> parse -> type check round trip" $ do
            rawSPL <- Lib.readUTF8File "example-programs/fac.spl"
            parseAndCheckDet rawSPL `shouldSatisfy` astEq (parseAndCheckDet (prettyPrint (parseAndCheckDet rawSPL)))
            --rawSPL <- Lib.readUTF8File "example-programs/tuple_increment.spl"
            --parseAndCheckDet rawSPL `shouldSatisfy` astEq (parseAndCheckDet (prettyPrint (parseAndCheckDet rawSPL)))
            rawSPL <- Lib.readUTF8File "example-programs/precedence_assoc.spl"
            parseAndCheckDet rawSPL `shouldSatisfy` astEq (parseAndCheckDet (prettyPrint (parseAndCheckDet rawSPL)))
            rawSPL <- Lib.readUTF8File "example-programs/last.spl"
            parseAndCheckDet rawSPL `shouldSatisfy` astEq (parseAndCheckDet (prettyPrint (parseAndCheckDet rawSPL)))
            rawSPL <- Lib.readUTF8File "example-programs/length.spl"
            parseAndCheckDet rawSPL `shouldSatisfy` astEq (parseAndCheckDet (prettyPrint (parseAndCheckDet rawSPL)))
            rawSPL <- Lib.readUTF8File "example-programs/concat.spl"
            parseAndCheckDet rawSPL `shouldSatisfy` astEq (parseAndCheckDet (prettyPrint (parseAndCheckDet rawSPL)))
            rawSPL <- Lib.readUTF8File "example-programs/fib.spl"
            parseAndCheckDet rawSPL `shouldSatisfy` astEq (parseAndCheckDet (prettyPrint (parseAndCheckDet rawSPL)))
            rawSPL <- Lib.readUTF8File "example-programs/precedence_parentheses.spl"
            parseAndCheckDet rawSPL `shouldSatisfy` astEq (parseAndCheckDet (prettyPrint (parseAndCheckDet rawSPL)))
            rawSPL <- Lib.readUTF8File "example-programs/ugly_style.spl"
            parseAndCheckDet rawSPL `shouldSatisfy` astEq (parseAndCheckDet (prettyPrint (parseAndCheckDet rawSPL)))
            rawSPL <- Lib.readUTF8File "example-programs/scopes.spl"
            parseAndCheckDet rawSPL `shouldSatisfy` astEq (parseAndCheckDet (prettyPrint (parseAndCheckDet rawSPL)))
            rawSPL <- Lib.readUTF8File "example-programs/tct.spl"
            parseAndCheckDet rawSPL `shouldSatisfy` astEq (parseAndCheckDet (prettyPrint (parseAndCheckDet rawSPL)))
            rawSPL <- Lib.readUTF8File "example-programs/tcthard.spl"
            parseAndCheckDet rawSPL `shouldSatisfy` astEq (parseAndCheckDet (prettyPrint (parseAndCheckDet rawSPL)))
            rawSPL <- Lib.readUTF8File "example-programs/tctpoly.spl"
            parseAndCheckDet rawSPL `shouldSatisfy` astEq (parseAndCheckDet (prettyPrint (parseAndCheckDet rawSPL)))
            rawSPL <- Lib.readUTF8File "example-programs/tctpolywithinfun.spl"
            parseAndCheckDet rawSPL `shouldSatisfy` astEq (parseAndCheckDet (prettyPrint (parseAndCheckDet rawSPL)))
            rawSPL <- Lib.readUTF8File "example-programs/tctstatements.spl"
            parseAndCheckDet rawSPL `shouldSatisfy` astEq (parseAndCheckDet (prettyPrint (parseAndCheckDet rawSPL)))
            rawSPL <- Lib.readUTF8File "example-programs/tctpolyreturn.spl"
            parseAndCheckDet rawSPL `shouldSatisfy` astEq (parseAndCheckDet (prettyPrint (parseAndCheckDet rawSPL)))
            rawSPL <- Lib.readUTF8File "example-programs/tctfields.spl"
            parseAndCheckDet rawSPL `shouldSatisfy` astEq (parseAndCheckDet (prettyPrint (parseAndCheckDet rawSPL)))
            rawSPL <- Lib.readUTF8File "example-programs/tctannotated.spl"
            parseAndCheckDet rawSPL `shouldSatisfy` astEq (parseAndCheckDet (prettyPrint (parseAndCheckDet rawSPL)))
