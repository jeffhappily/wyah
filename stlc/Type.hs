module Type where

data Type
  = TInt
  | TBool
  | TArr Type Type
  deriving (Eq, Show)
