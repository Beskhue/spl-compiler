{-|
Module: NFA
Description: NFA implementation
Copyright: (c) Thomas Churchman, 2017
License: MIT
Maintainer: thomas@kepow.org
Stability: experimental

An implementation of NFAs.

Possible improvements:
Implement using Data.Set, to enforce uniqueness of states
and make searching for elements more efficient (due to binary
search).
-}


module Data.NFA where

import Control.Monad
import Data.List
import Data.DFA as DFA

{-|
  List (set) of states
  List (set) of transitions
  Initial state
  Accepting states
 -}
data NFA a = NFA
                [a]
                [Transition a]
                a
                [a]
                deriving (Eq, Show)

{-|
  A transition is either a transition from one state to another given a char
  or a transition from one state to another given no input
-}
data Transition a = Transition a Char a
                  | EmptyTransition a a
                    deriving (Eq, Show)

-- |Test whether state 'state' is an accepting state in an NFA
isAccepting :: Eq a => NFA a -> a -> Bool
isAccepting (NFA _ _ _ acceptingStates) state = elem state acceptingStates

-- |Test whether the current state, input character and next state are consistent with the transition
validTransition :: Eq a => Transition a -> a -> Maybe Char -> a -> Bool
validTransition (Transition s1 char s2) s1' (Just char') s2' =
    (s1 == s2')
    && (char == char')
    && (s2 == s2')
validTransition (EmptyTransition s1 s2) s1' Nothing s2' =
    (s1 == s1')
    && (s2 == s2')
validTransition _ _ _ _ = False

-- |Test whether the current state and input character can be applied to the given transition
validTransitionCondition :: Eq a => Transition a -> a -> Maybe Char -> Bool
validTransitionCondition (Transition s1 char _) s1' (Just char') =
    (s1 == s1')
    && (char == char')
validTransitionCondition (EmptyTransition s1 _) s1' Nothing = (s1 == s1')
validTransitionCondition _ _ _ = False

-- |Get the initial state of an NFA
getInitialState :: NFA a -> a
getInitialState (NFA _ _ s _) = s

-- |Get the post state of a transition
getPostState :: Transition a -> a
getPostState (Transition _ _ s) = s
getPostState (EmptyTransition _ s) = s

-- |Get the list of states we can transition to given this NFA in the given state, and this input
transition :: Eq a => NFA a -> a -> Maybe Char -> [a]
transition (NFA _ transitions _ _) state input = map getPostState [t | t <- transitions, validTransitionCondition t state input]

-- |Get the full list of states we can get to by performing zero or more lambda (no-input) moves
lambdaClosure :: Eq a => NFA a -> [a] -> [a]
lambdaClosure nfa states = lambdaClosure' nfa states states
    where lambdaClosure' :: Eq a => NFA a -> [a] -> [a] -> [a]
          lambdaClosure' nfa (s:ss) closure = lambdaClosure' nfa (ss ++ newly_accessible) (closure ++ newly_accessible)
            where
                newly_accessible = [accessible | accessible <- transition nfa s Nothing, not $ elem accessible closure]
          lambdaClosure' nfa [] closure = closure

-- |Get the full list of states we can get to by consuming input character
transitions :: Eq a => NFA a -> [a] -> Char -> [a]
transitions nfa states input = nub [accessible | s <- states, accessible <- transition nfa s (Just input)]

-- |Test whether a sequence is valid given an NFA
evaluate :: Eq a => NFA a -> String -> Bool
evaluate nfa inputString = any (isAccepting nfa) (accessible nfa acc inputString)
    where
        acc = lambdaClosure nfa [getInitialState nfa]
        accessible :: Eq a => NFA a -> [a] -> String -> [a]
        accessible nfa states (i:ii) = accessible nfa (lambdaClosure nfa (transitions nfa states i)) ii
        accessible nfa states [] = states

-- |Finds all substrings (expanding to the right) that are recognized by the NFA
matches :: Eq a => NFA a -> String -> [String]
matches nfa inputString = snd (accessible nfa acc inputString "" [])
    where
        acc = lambdaClosure nfa [getInitialState nfa]
        accessible :: Eq a => NFA a -> [a] -> String -> String -> [String] -> ([a], [String])
        accessible nfa states (i:ii) consumed accepting = accessible
            nfa
            (lambdaClosure nfa (transitions nfa states i))
            ii
            (consumed ++ [i])
            (if any (isAccepting nfa) states then accepting ++ [consumed] else accepting)
        accessible nfa states [] consumed accepting =
            (
                states,
                if any (isAccepting nfa) states then accepting ++ [consumed] else accepting
            )

-- |Finds the longest substring recognized by the NFA (the substrings expand to the right, i.e. they match "^regex")
longestMatch :: Eq a => NFA a -> String -> Maybe String
longestMatch nfa inputString = if length matches' > 0
    then Just $ last matches'
    else Nothing
    where
        matches' = matches nfa inputString