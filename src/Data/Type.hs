{-|
Module: Data.Type
Description: SPL types
Copyright: (c) Thomas Churchman, 2017
License: MIT
Maintainer: thomas@kepow.org
Stability: experimental

-}


module Data.Type where

-- |The possible types
data Type = TVar String
          | TBool
          | TInt
          | TChar
          | TList Type
          | TTuple Type Type
          | TFunction [Type] Type
          | TPointer Type
          | TVoid
          | TType
          | TClass Type
          | TClassIdentifier String
            deriving (Show, Eq, Ord)

-- |A type scheme (polytype): a type with a list of bound type variables (the type variables not bound are still free)
data Scheme = Scheme [String] Type
              deriving (Show)
