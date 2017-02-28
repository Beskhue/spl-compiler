{-|
Module: NFA
Description: NFA implementation
Copyright: (c) Thomas Churchman, 2017
License: MIT
Maintainer: thomas@kepow.org
Stability: experimental

An implementation of NFAs.
-}


module Data.NFA where

import Control.Monad
import Data.Set (Set)

{-|
  List (set) of states
  List (set) of transitions
  Initial state
  Accepting states
 -}
data NFA a = NFA
                (Set a)
                [Transition a]
                a
                (Set a)
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

-- |Get the full list of states we can get to by performing zero or more epsilon (no-input) moves
epsilonClosure :: Eq a => NFA a -> [a] -> [a]
epsilonClosure nfa states = epsilonClosure' nfa states states
    where epsilonClosure' :: Eq a => NFA a -> [a] -> [a] -> [a]
          epsilonClosure' nfa (s:ss) closure = epsilonClosure' nfa (ss ++ newly_accessible) (closure ++ newly_accessible)
            where
                newly_accessible = [accessible | accessible <- transition nfa s Nothing, not $ elem accessible closure]
          epsilonClosure' nfa [] closure = closure

-- |Get the full list of states we can get to by consuming input character
transitions :: Eq a => NFA a -> [a] -> Char -> [a]
transitions nfa states input = nub [accessible | s <- states, accessible <- transition nfa s (Just input)]

-- |Test whether a sequence is valid given an NFA
testSequence :: Eq a => NFA a -> String -> Bool
testSequence nfa inputString = any (isAccepting nfa) (accessible nfa acc inputString)
    where
        acc = epsilonClosure nfa [getInitialState nfa]
        accessible :: Eq a => NFA a -> [a] -> String -> [a]
        accessible nfa states (i:ii) = accessible nfa (epsilonClosure nfa (transitions nfa states i)) ii
        accessible nfa states [] = states
