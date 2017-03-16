module Text.Regex.ThompsonTest where

import Test.Tasty.Hspec as HSpec
import Text.Regex.Thompson as Regex

spec_Regex :: Spec
spec_Regex = do
    describe "Regex.tokenize" $ do
        it "gives only TEOF for an empty string" $
            Regex.tokenize "" `shouldBe` [Regex.TEOF]
        it "tokenizes an input string" $
            Regex.tokenize "a+a*\\*\\\\" `shouldBe` [Regex.TChar 'a', Regex.TPlus, Regex.TChar 'a', Regex.TStar, Regex.TChar '*', Regex.TChar '\\', Regex.TEOF]
    describe "Regex.parse" $ do
        it "parses a single literal" $
            Regex.parse "a" `shouldBe` (Regex.Literal 'a')
        it "parses a group with a single literal" $
            Regex.parse "(a)" `shouldBe` (Regex.Group (Regex.Literal 'a'))
        it "parses concatenation" $
            Regex.parse "ab" `shouldBe` (Regex.Concat (Regex.Literal 'a') (Regex.Literal 'b'))
        it "parses a union" $
            Regex.parse "a|b" `shouldBe` (Regex.Union (Regex.Literal 'a') (Regex.Literal 'b'))
        it "parses an escaped control character" $
            Regex.parse "a\\*" `shouldBe` (Regex.Concat (Regex.Literal 'a') (Regex.Literal '*'))
        it "parses a complex regex" $
            Regex.parse "(ab|cd)?e*" `shouldBe` (Regex.Concat
                    (
                        Regex.Option (Regex.Group(
                                Regex.Union
                                (Regex.Concat (Regex.Literal 'a')  (Regex.Literal 'b'))
                                (Regex.Concat (Regex.Literal 'c')  (Regex.Literal 'd'))
                            )
                        )
                    )
                    (
                        Regex.Star (Regex.Literal 'e')
                    )
                )
    describe "Regex.evaluate" $ do
        it "recognizes a trivial regex" $
            let a = Regex.compile "a" in do
                Regex.evaluate a "a" `shouldBe` True
                Regex.evaluate a "b" `shouldBe` False
                Regex.evaluate a "" `shouldBe` False
        it "recognizes a trivial union" $
            let a = Regex.compile "a|b" in do
                Regex.evaluate a "a" && Regex.evaluate a "b" `shouldBe` True
                Regex.evaluate a "c" `shouldBe` False
                Regex.evaluate a "ab" `shouldBe` False
        it "recognizes a complex regex" $
            let a = Regex.compile "(ab|cd)?e*" in do
                Regex.evaluate a "" `shouldBe` True
                Regex.evaluate a "e" `shouldBe` True
                Regex.evaluate a "eee" `shouldBe` True
                Regex.evaluate a "ae" `shouldBe` False
                Regex.evaluate a "ab" `shouldBe` True
                Regex.evaluate a "abee" `shouldBe` True
                Regex.evaluate a "cd" `shouldBe` True
                Regex.evaluate a "abeeb" `shouldBe` False
        it "recognizes exactly the characters in a character set" $
            let a = Regex.compile "[abch-lX-Zq0-9r]" in
                let chars = ['a', 'b', 'c'] ++ ['h' .. 'l'] ++ ['X' .. 'Z'] ++ ['q'] ++ ['0' .. '9'] ++ ['r'] in do
                    all (== True) (map (Regex.evaluate a . (: "")) chars) `shouldBe` True
                    Regex.evaluate a "ab" `shouldBe` False
                    Regex.evaluate a "" `shouldBe` False
        it "recognizes exactly the characters not in a negative character set" $
            let a = Regex.compile "[^abch-lX-Zq0-9r]" in
                let chars = ['a', 'b', 'c'] ++ ['h' .. 'l'] ++ ['X' .. 'Z'] ++ ['q'] ++ ['0' .. '9'] ++ ['r'] in do
                    all (== True) (map (Regex.evaluate a . (: "")) chars) `shouldBe` False
                    Regex.evaluate a "A" `shouldBe` True
                    Regex.evaluate a "ab" `shouldBe` False
                    Regex.evaluate a "" `shouldBe` False
        it "recognizes all characters for ." $
            let a = Regex.compile "..." in do
                Regex.evaluate a "a-" `shouldBe` False
                Regex.evaluate a "a-0" `shouldBe` True
                Regex.evaluate a "a-0^" `shouldBe` False
        it "recognizes everything for .*" $
            let a = Regex.compile ".*" in do
                Regex.evaluate a "" `shouldBe` True
                Regex.evaluate a "abcdABCD01234" `shouldBe` True
                Regex.evaluate a "^" `shouldBe` True
                Regex.evaluate a " " `shouldBe` True
                Regex.evaluate a "\n" `shouldBe` True
        it "recognizes lambda (empty word)" $ do
            let a = Regex.compile "" in do
                Regex.evaluate a "" `shouldBe` True
                Regex.evaluate a "a" `shouldBe` False
                Regex.evaluate a "abc" `shouldBe` False
            let a = Regex.compile "()|()" in do
                Regex.evaluate a "" `shouldBe` True
                Regex.evaluate a "a" `shouldBe` False
        it "handles non-ASCII characters" $
            let a = Regex.compile "молоко" in do
                Regex.evaluate a "молоко" `shouldBe` True
                Regex.evaluate a "moloko" `shouldBe` False
        it "recognizes the letter class" $
            let a = Regex.compile "\\w+" in do
                Regex.evaluate a "moloko" `shouldBe` True
                Regex.evaluate a "молоко" `shouldBe` True
                Regex.evaluate a "м0локо" `shouldBe` False
                Regex.evaluate a "√" `shouldBe` False -- not a letter
    describe "Regex.longestMatch" $ do
        it "recognizes the longest match is not necessarily the entire string" $
            let a = Regex.compile "(aaa)+" in do
                Regex.longestMatch a "aaaaaaaa" `shouldBe` Just "aaaaaa"
                Regex.longestMatch a "aa" `shouldBe` Nothing
        it "recognizes the longest match in a complex regex" $
            let a = Regex.compile "Hello( world)*" in do
                Regex.longestMatch a "Hello world" `shouldBe` Just "Hello world"
                Regex.longestMatch a "Hello wor" `shouldBe` Just "Hello"
                Regex.longestMatch a "Hello world world world" `shouldBe` Just "Hello world world world"
    describe "Regex.shortestMatch" $ do
        it "recognizes the shortest match is not necessarily an empty string" $
            let a = Regex.compile "(aaa)+" in do
                Regex.shortestMatch a "aaaaaaaa" `shouldBe` Just "aaa"
                Regex.shortestMatch a "aa" `shouldBe` Nothing
        it "recognizes the shortest match in a complex regex" $
            let a = Regex.compile "Hello( world)*" in do
                Regex.shortestMatch a "Hello world" `shouldBe` Just "Hello"
                Regex.shortestMatch a "Hello wor" `shouldBe` Just "Hello"
                Regex.shortestMatch a "Hello world world world" `shouldBe` Just "Hello"
        it "recognizes the shortest match as the empty string" $
            let a = Regex.compile "(a|b)*" in do
                Regex.shortestMatch a "The quick brown fox jumped over the lazy dog" `shouldBe` Just ""
                Regex.shortestMatch a "" `shouldBe` Just ""
        it "only matches entire string when using end of string anchors" $
            let a = Regex.compile "a*$" in do
                Regex.shortestMatch a "aaaa" `shouldBe` Just "aaaa"
                Regex.shortestMatch a "" `shouldBe` Just ""
                Regex.shortestMatch a "b" `shouldBe` Nothing
    describe "Regex.matches" $ do
        it "finds no matches if there are none" $
            let a = Regex.compile "a+b+" in do
                Regex.matches a "acbbbb" `shouldBe` []
                Regex.matches a "bbbbb" `shouldBe` []
                Regex.matches a "" `shouldBe` []
        it "finds all matches" $
            let a = Regex.compile "a+|b*" in do
                Regex.matches a "c" `shouldBe` [""]
                Regex.matches a "abc" `shouldBe` ["", "a", "ab"]
                Regex.matches a "aabbc" `shouldBe` ["", "a", "aa", "aab", "aabb"]
