module Data.DFA where

import Data.List

{-|
  List (set) of states
  List (set) of transitions
  Initial state
  Accepting states
 -}
data DFA a = DFA {
                states :: [a],
                delta :: a -> Char -> a,
                initialState :: a,
                accepting :: a -> Bool
}

-- |Test whether a sequence is valid given a DFA
evaluate :: Eq a => DFA a -> String -> Bool
evaluate (DFA states delta initialState accepting) inputString = accepting $ foldl delta initialState inputString