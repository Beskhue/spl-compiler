module Lexer.LexerTest where

import Test.Tasty.Hspec as HSpec
import Data.Token
import Data.Pos
import Lexer.Lexer as Lexer

spec_Lexer :: Spec
spec_Lexer =
    describe "Lexer.lex" $ do
        it "gives only TEOF for an empty string" $
            getTokens (Lexer.lexDet "test" "") `shouldBe`  [TEOF]
        it "gives only TEOF for code within a comment" $
            getTokens (Lexer.lexDet "test" "/* Int a = 5; */") `shouldBe` [TEOF]
        it "gives only TEOF for whitespace (including line breaks)" $
            getTokens (Lexer.lexDet "test" " \t\t\n\n\f\f  ") `shouldBe` [TEOF]
        it "lexes open code that is between two block comments" $
            getTokens (Lexer.lexDet "test" "/* Comment1 */ a /* Comment2 */") `shouldBe` [TIdentifier "a", TEOF]
        it "lexes keywords as keywords and identifiers as identifiers" $
            getTokens (Lexer.lexDet "test" "var if else while return vaa iff eelse retur") `shouldBe` [
                TKeyword KVar, TKeyword KIf, TKeyword KElse, TKeyword KWhile, TKeyword KReturn,
                TIdentifier "vaa", TIdentifier "iff", TIdentifier "eelse", TIdentifier "retur",
                TEOF]
        it "lexes field identifiers as fields" $
            getTokens (Lexer.lexDet "test" "a.hd.fst.snd.tl") `shouldBe` [
                TIdentifier "a",
                TField FHd, TField FFst, TField FSnd, TField FTl,
                TEOF]
        it "handles unicode letters when lexing identifiers" $
            getTokens (Lexer.lexDet "test" "молоко мо_loko9") `shouldBe` [
                TIdentifier "молоко", TIdentifier "мо_loko9", TEOF]
        it "lexes numbers as integer constants" $
            getTokens (Lexer.lexDet "test" "1234567890") `shouldBe` [TConstant (CInt 1234567890), TEOF]
        it "lexes hexadecimal numbers as integer constants" $
            getTokens (Lexer.lexDet "test" "0xFF") `shouldBe` [TConstant (CInt 255), TEOF]
        it "lexes True/False as boolean constants" $
            getTokens (Lexer.lexDet "test" "True False") `shouldBe` [
                TConstant (CBool True), TConstant (CBool False),
                TEOF]
        it "lexes characters as char constants" $
            getTokens (Lexer.lexDet "test" "'a' 'ь' '0'") `shouldBe` [
                TConstant (CChar 'a'), TConstant (CChar 'ь'), TConstant (CChar '0'), TEOF]
        it "lexes non-ASCII characters as char constants" $
            getTokens (Lexer.lexDet "test" "'ь'") `shouldBe` [TConstant (CChar 'ь'), TEOF]
        it "lexes characters with special meaning in the syntax as char constants" $
            getTokens (Lexer.lexDet "test" "'+' '!' ';'") `shouldBe` [
                TConstant (CChar '+'), TConstant (CChar '!'), TConstant (CChar ';'), TEOF]
        it "lexes an apostrophe as a character if and only if it is escaped" $ do
            getTokens (Lexer.lexDet "test" "'''") `shouldBe` []
            getTokens (Lexer.lexDet "test" "'\\''") `shouldBe` [TConstant (CChar '\''), TEOF]
        it "lexes an empty list as a constant empty list" $
            getTokens (Lexer.lexDet "test" "[]") `shouldBe` [TConstant CEmptyList, TEOF]
        it "lexes types" $
            getTokens (Lexer.lexDet "test" "Int Bool Char Void") `shouldBe` [
                TType TypeInt, TType TypeBool, TType TypeChar, TType TypeVoid,
                TEOF]
        it "lexes operators" $
            getTokens (Lexer.lexDet "test" "= + * / % == < > <= >= != && || : !") `shouldBe` [
                TOperator OAssignment, TOperator OPlus, TOperator OMultiply, TOperator ODivide, TOperator OMod,
                TOperator OEq, TOperator OLT, TOperator OGT, TOperator OLTE, TOperator OGTE, TOperator ONEq,
                TOperator OAnd, TPunctuator PPipe, TPunctuator PPipe, TOperator OConcat, TOperator ONeg,
                TEOF]
        it "lexes punctuators" $
            getTokens (Lexer.lexDet "test" "; ( ) { } [ ] , -> - ::") `shouldBe` [
                TPunctuator PSeparator, TPunctuator PParenOpen, TPunctuator PParenClose,
                TPunctuator PBraceOpen, TPunctuator PBraceClose,
                TPunctuator PSquareBracketOpen, TPunctuator PSquareBracketClose, TPunctuator PComma,
                TPunctuator PMapTo, TPunctuator PMinus, TPunctuator PFunType,
                TEOF]
        it "keeps track of source position" $
            Lexer.lexDet "test" "a\n\na a a/*\n\naaa*/a" `shouldBe` [
                TP (TIdentifier "a") (Pos "test" 1 1),
                TP (TIdentifier "a") (Pos "test" 3 1),
                TP (TIdentifier "a") (Pos "test" 3 3),
                TP (TIdentifier "a") (Pos "test" 3 5),
                TP (TIdentifier "a") (Pos "test" 5 6),
                TP TEOF (Pos "test" 5 7)]
