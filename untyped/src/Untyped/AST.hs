module Untyped.AST where

type Name = String

data Expr
  = Var Name
  | App Expr Expr
  | Lam Name Expr
  | Lit Lit

data Lit
  = LInt Int

instance Show Lit where
  show (LInt i) = show i
