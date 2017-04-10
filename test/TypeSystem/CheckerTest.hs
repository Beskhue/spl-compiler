module TypeSystem.CheckerTest where

import qualified Data.Map as Map
import Test.Tasty.Hspec as HSpec
import qualified Data.Pos as Pos
import qualified Data.AST as AST
import Data.Pos
import TypeSystem.Checker as Checker

spec_Checker :: Spec
spec_Checker =
    describe "Checker.typeInferenceExpr" $
        it "infers that an int expression is of type int" $
            let p =  Pos { sourceName = "test", line = 1, column = 1 } in
                let a = (AST.ExprConstant (AST.ConstInt 5, p), p) in
                    fst (Checker.runTInf (Checker.typeInferenceExpr Map.empty a)) `shouldBe` Right Checker.TInt
