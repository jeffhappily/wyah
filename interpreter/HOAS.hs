{-# LANGUAGE GADTs #-}

module HOAS (Expr(..)) where
-- Higher Order Abstract Syntax

data Expr a where
  Lift :: a                       -> Expr a
  Tup  :: Expr a -> Expr b        -> Expr (a, b)
  Lam  :: (Expr a -> Expr b)      -> Expr (a -> b)
  App  :: Expr (a -> b) -> Expr a -> Expr b
  Fix  :: Expr (a -> a)           -> Expr a

id :: Expr (a -> a)
id = Lam (\x -> x)

tr :: Expr (a -> b -> a)
tr = Lam (\x -> (Lam (\y -> x)))

fl :: Expr (a -> b -> b)
fl = Lam (\x -> (Lam (\y -> y)))
