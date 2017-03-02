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

data Input = IChar Char | ILambda | IEOS

instance Show a => Show (NFA a) where
    show (NFA states trans init accs) =
        "States: " ++ show states
        ++ "\nInitial state: " ++ show init
        ++ "\nAccepting states: " ++ show accs
        ++ "\nTransitions: " ++ show trans

{-|
  A transition is either a transition from one state to another given a char
  or a transition from one state to another given no input

  1. A transition from one state to given a character
  2. A transition from one state to another if the given character evaluates the function to true
  3. A transition from one state to another given no input
  4. A transition from one state to another given we're at the end of the string
-}
data Transition a = Transition a Char a
                  | ConditionalTransition a (Char -> Bool) a
                  | EmptyTransition a a
                  | EOSTransition a a

instance Show a => Show (Transition a) where
    show (Transition s1 c s2) = show s1 ++ " -" ++ [c] ++ "> " ++ show s2
    show (ConditionalTransition s1 f s2) = show s1 ++ "-conditional>" ++ show s2
    show (EmptyTransition s1 s2) = show s1 ++ "-lambda>" ++ show s2
    show (EOSTransition s1 s2) = show s1 ++ "-eos>" ++ show s2

-- |Test whether state 'state' is an accepting state in an NFA
isAccepting :: Eq a => NFA a -> a -> Bool
isAccepting (NFA _ _ _ acceptingStates) state = elem state acceptingStates

-- |Test whether the current state, input character and next state are consistent with the transition
validTransition :: Eq a => Transition a -> a -> Input -> a -> Bool
validTransition (Transition s1 char s2) s1' (IChar char') s2' =
    (s1 == s2')
    && (char == char')
    && (s2 == s2')
validTransition (ConditionalTransition s1 f s2) s1' (IChar char') s2' =
    (s1 == s2')
    && f char'
    && (s2 == s2')
validTransition (EmptyTransition s1 s2) s1' ILambda s2' =
    (s1 == s1')
    && (s2 == s2')
validTransition (EOSTransition s1 s2) s1' IEOS s2' =
    (s1 == s1')
    && (s2 == s2')
validTransition _ _ _ _ = False

-- |Test whether the current state and input character can be applied to the given transition
validTransitionCondition :: Eq a => Transition a -> a -> Input -> Bool
validTransitionCondition (Transition s1 char _) s1' (IChar char') =
    (s1 == s1')
    && (char == char')
validTransitionCondition (ConditionalTransition s1 f _) s1' (IChar char ) =
    (s1 == s1')
    && f char
validTransitionCondition (EmptyTransition s1 _) s1' ILambda = (s1 == s1')
validTransitionCondition (EOSTransition s1 _) s1' IEOS = (s1 == s1')
validTransitionCondition _ _ _ = False

-- |Get the initial state of an NFA
getInitialState :: NFA a -> a
getInitialState (NFA _ _ s _) = s

-- |Get the post state of a transition
getPostState :: Transition a -> a
getPostState (Transition _ _ s) = s
getPostState (ConditionalTransition _ _ s) = s
getPostState (EmptyTransition _ s) = s
getPostState (EOSTransition _ s) = s

-- |Get the list of states we can transition to given this NFA in the given state, and this input
transition :: Eq a => NFA a -> a -> Input -> [a]
transition (NFA _ transitions _ _) state input = map getPostState [t | t <- transitions, validTransitionCondition t state input]

-- |Get the full list of states we can get to by performing zero or more lambda (no-input) moves
lambdaClosure :: Eq a => NFA a -> [a] -> [a]
lambdaClosure nfa states = lambdaClosure' nfa states states
    where lambdaClosure' :: Eq a => NFA a -> [a] -> [a] -> [a]
          lambdaClosure' nfa (s:ss) closure = lambdaClosure' nfa (ss ++ newly_accessible) (closure ++ newly_accessible)
            where
                newly_accessible = [accessible | accessible <- transition nfa s ILambda, not $ elem accessible closure]
          lambdaClosure' nfa [] closure = closure

-- |Get the full list of states we can get to by consuming input character
transitions :: Eq a => NFA a -> [a] -> Char -> [a]
transitions nfa states input = nub [accessible | s <- states, accessible <- transition nfa s (IChar input)]

-- |Get the full list of states we can get to by consuming an EOS
eosTransitions :: Eq a => NFA a -> [a] -> [a]
eosTransitions nfa states = nub [accessible | s <- states, accessible <- transition nfa s IEOS]

-- |Test whether a sequence is valid given an NFA
evaluate :: Eq a => NFA a -> String -> Bool
evaluate nfa inputString = any (isAccepting nfa) (accessible nfa acc inputString)
    where
        acc = lambdaClosure nfa [getInitialState nfa]
        accessible :: Eq a => NFA a -> [a] -> String -> [a]
        accessible nfa states (i:ii) = accessible nfa (lambdaClosure nfa (transitions nfa states i)) ii
        accessible nfa states [] = lambdaClosure nfa (eosTransitions nfa states)

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
                eosStates,
                if any (isAccepting nfa) eosStates then accepting ++ [consumed] else accepting
            )
            where eosStates = lambdaClosure nfa (eosTransitions nfa states)

-- |Finds the longest substring recognized by the NFA (the substrings expand to the right, i.e. they match "^regex")
longestMatch :: Eq a => NFA a -> String -> Maybe String
longestMatch nfa inputString = if length matches' > 0
    then Just $ last matches'
    else Nothing
    where
        matches' = matches nfa inputString

-- |Finds the shortest substring recognized by the NFA (the substrings expand to the right, i.e. they match "^regex")
shortestMatch :: Eq a => NFA a -> String -> Maybe String
shortestMatch nfa inputString = if length matches' > 0
    then Just $ head matches'
    else Nothing
    where
        matches' = matches nfa inputString
