module TypeSystem.CheckerTest where

import qualified Data.Map as Map
import Test.Tasty.Hspec as HSpec
import qualified Data.Pos as Pos
import qualified Data.AST as AST
import Data.Pos
import TypeSystem.Checker as Checker

spec_Lexer :: Spec
spec_Lexer =
    describe "Checker.typeInferenceExpr" $
        it "parses an int expression as an int" $
            let p =  Pos { sourceName = "test", line = 1, column = 1 } in
                let a = (AST.ExprConstant (AST.ConstInt 5, p), p) in
                    Checker.runTInf (Checker.typeInferenceExpr Map.empty a) `shouldBe` Checker.TInt
