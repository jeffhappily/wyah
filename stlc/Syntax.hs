module Syntax where

import Type (Type)

type Name = String

data Expr
  = Var Name
  | App Expr Expr
  | Lam Name Type Expr
  | If Expr Expr Expr
  | Lit Lit
  deriving (Eq, Show)

data Lit
  = LInt Int
  | LBool Bool
  deriving (Eq, Show)
