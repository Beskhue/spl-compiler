module Lexer.LexerTest where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit as HU
import Test.Tasty.Hspec as HSpec
import Data.Token
import Data.Pos
import Lexer.Lexer as Lexer

spec_Lexer :: Spec
spec_Lexer = do
    describe "Lexer.lex" $ do
        it "gives only TEOF for an empty string" $ do
            getTokens (Lexer.lexDet "test" "") `shouldBe`  [TEOF]
