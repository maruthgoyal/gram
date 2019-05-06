module Type where

newtype TVar = TV String deriving (Show, Eq, Ord)

data Type = TVar TVar
          | TInt
          | TBool
          | TFunc Type Type
          deriving (Show, Eq, Ord)

data Scheme = ForAll [String] Type
