{-|
Module: Pos
Description: Keeps track of source positions.
Copyright: (c) Thomas Churchman, 2017
License: MIT
Maintainer: thomas@kepow.org
Stability: experimental
-}

module Data.Pos where

data Pos = Pos { sourceName :: String, line :: !Int, column :: !Int }

instance Show Pos where
    show (Pos sourceName line column)
        | null sourceName = "'" ++ sourceName ++ "':(" ++ show line ++ "," ++ show column ++ ")"
        | otherwise = "(" ++ show line ++ "," ++ show column ++ ")"

increaseLine :: Pos -> Int -> Pos
increaseLine (Pos sourceName line column) n = Pos sourceName (line + n) column

increaseColumn :: Pos -> Int -> Pos
increaseColumn (Pos sourceName line column) n = Pos sourceName line (column + n)

setLine :: Pos -> Int -> Pos
setLine (Pos sourceName _ column) n = Pos sourceName n column

setColumn :: Pos -> Int -> Pos
setColumn (Pos sourceName line _) n = Pos sourceName line n
