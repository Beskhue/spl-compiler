module TypeSystem.CheckerTest where

import Data.Either
import Test.Tasty.Hspec as HSpec
import qualified Parser.SPLParser as SPLParser
import qualified Lexer.Lexer as Lexer
import TypeSystem.Checker as Checker

expr = SPLParser.parseDet' SPLParser.pExpression . Lexer.lexDet "test"
emptyTInfExpr = Checker.typeInference Checker.tInfExpr Checker.emptyMap
onlyTypeEmptyTInfExpr = Checker.onlyType . emptyTInfExpr

spec_Checker :: Spec
spec_Checker =
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


