-- | Stack data structure and associated operations
-- A stack is a basic data structure that can be logically thought as linear structure represented by a real physical stack or pile, a structure where insertion and deletion of items takes place at one end called top of the stack.
--
-- In other words, a 'Stack' is an abstract data type that serves as a collection of elements, with two principal operations: 'stackPush', which adds an element to the collection, and 'stackPop', which removes the most recently added element that was not yet removed.
--
-- <<https://upload.wikimedia.org/wikipedia/commons/b/b4/Lifo_stack.png>>
--
-- See also <https://en.wikipedia.org/wiki/Stack_(abstract_data_type)>

{-

https://hackage.haskell.org/package/Stack

Copyright Author name here (c) 2016

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Author name here nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

-}

module Data.Stack (
    Stack,
    stackNew,
    stackPush,
    stackPeek,
    stackPop,
    stackIsEmpty,
    stackSize,
    stackToList,
  )
  where

import Numeric.Natural

-- | Abstract Stack data type
data Stack a = Stack !Natural [a] deriving (Read,Show)

-- | /O(1)/. Create new empty Stack
stackNew :: Stack a
stackNew = Stack 0 []

-- | /O(1)/. Push item onto Stack
--
-- > (∀x)(∀s)(stackPop (stackPush s x) == Just (s,x))
stackPush :: Stack a -> a -> Stack a
stackPush (Stack sz items) item = Stack (succ sz) (item : items)

-- | /O(1)/. Pop most recently added item without removing from the Stack
--
-- > stackPeek stackNew == Nothing
-- > (∀x)(∀s)(stackPeek (stackPush s x) == Just x)
-- > (∀s)(stackPeek s == fmap snd (stackPop s))
stackPeek :: Stack a -> Maybe a
stackPeek (Stack _ []) = Nothing
stackPeek (Stack _ items) = Just (head items)

-- | /O(1)/. Pop most recently added item from Stack
--
-- > stackPop stackNew == Nothing
-- > (∀x)(∀s)(stackPop (stackPush s x) == Just (s,x))
stackPop :: Stack a -> Maybe (Stack a, a)
stackPop (Stack _ []) = Nothing
stackPop (Stack sz items) = Just (Stack (pred sz) (tail items), head items)

-- | /O(1)/. Test if stack is empty
--
-- > stackIsEmpty stackNew == True
-- > (∀x)(∀s)(stackIsEmpty (stackPush s x) == True)
-- > (∀s)((stackSize s == 0) ⇔ (stackIsEmpty s == True))
stackIsEmpty :: Stack a -> Bool
stackIsEmpty (Stack _ []) = True
stackIsEmpty (Stack _ _)  = False

-- | /O(1)/. Compute number of elements contained in the Stack
--
-- > stackSize stackNew == 0
-- > (∀x)(∀s)((stackSize s == n) ⇒ (stackSize (stackPush s x) == n+1))
stackSize :: Stack a -> Natural
stackSize (Stack sz _) = sz

stackToList :: Stack a -> [a]
stackToList (Stack _ l) = l
