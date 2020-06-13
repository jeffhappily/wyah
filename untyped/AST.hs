module AST where

type Name = String

data Expr
  = Var Name
  | App Expr Expr
  | Lam Name Expr
  | Lit Lit

instance Show Expr where
  show (Var n) = n
  show (App a b) = "(" <> show a <> " " <> show b <> ")"
  show (Lam a b) = "(" <> "\\" <> a <> " . " <> show b <> ")"
  show (Lit l) = show l

data Lit
  = LInt Int
  | LBool Bool

instance Show Lit where
  show (LInt i) = show i
  show (LBool b) = show b
