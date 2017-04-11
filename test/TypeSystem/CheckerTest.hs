module TypeSystem.CheckerTest where

import Data.Either
import Test.Tasty.Hspec as HSpec
import qualified Data.Pos as Pos
import qualified Data.AST as AST
import Data.Pos
import qualified Parser.SPLParser as SPLParser
import qualified Lexer.Lexer as Lexer
import TypeSystem.Checker as Checker

expr = SPLParser.parseDet' SPLParser.pExpression . Lexer.lexDet "test"
emptyTInfExpr = Checker.typeInference Checker.tInfExpr Checker.emptyMap
onlyTypeEmptyTInfExpr = Checker.onlyType . emptyTInfExpr

spec_Checker :: Spec
spec_Checker =
    describe "Checker.tInfExpr" $ do
        it "infers that an int expression is of type int" $
            let p =  Pos { sourceName = "test", line = 1, column = 1 } in
                let a = (AST.ExprConstant (AST.ConstInt 5, p), p) in
                    onlyTypeEmptyTInfExpr a `shouldBe` Right Checker.TInt
        it "infers that '1;'  is of type TInt" $
            onlyTypeEmptyTInfExpr (expr  "1;") `shouldBe` Right Checker.TInt
        it "infers that '1 + 2' is of type TInt" $
            onlyTypeEmptyTInfExpr (expr  "1 + 2;") `shouldBe` Right Checker.TInt
        it "infers that '(1 : []) : []' is of type [[TInt]]" $
            onlyTypeEmptyTInfExpr (expr "(1 : []) : [];") `shouldBe` Right (Checker.TList (Checker.TList Checker.TInt))
        it "infers that 'False : True : []' is of type [TBool]" $
            onlyTypeEmptyTInfExpr (expr "True : False : [];") `shouldBe` Right (Checker.TList TBool)
        it "infers that '[]' is a list of variable type" $
            onlyTypeEmptyTInfExpr (expr "[];") `shouldSatisfy` (\t ->
                case t of
                    Right (Checker.TList (Checker.TVar _)) -> True
                    _ -> False)
        it "produces an error for '1 : 'a' : []' " $
            onlyTypeEmptyTInfExpr (expr "1 : 'a' : [];") `shouldSatisfy` isLeft
        it "produces an error for ''a' + 'b''" $
            onlyTypeEmptyTInfExpr (expr  "'a' + 'b';") `shouldSatisfy` isLeft


