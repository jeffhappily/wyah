module CallByNeed (
  test1,
  test2,
) where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)

data Expr
  = EVar String
  | ELam String Expr
  | EApp Expr Expr
  | EInt Integer
  | EBool Bool
  | EFix Expr
  deriving (Show)

data Value
  = VBool Bool
  | VInt Integer
  | VClosure (Thunk -> IO Value)

instance Show Value where
  show (VBool b) = show b
  show (VInt n) = show n
  show (VClosure _) = "<<closure>>"

type Thunk = () -> IO Value

type Env = [(String, IORef Thunk)]

update :: IORef Thunk -> Value -> IO ()
update ref v = do
  writeIORef ref (\() -> return v)
  return ()

force :: IORef Thunk -> IO Value
force ref = do
  th <- readIORef ref
  v <- th ()
  update ref v
  return v

mkThunk :: Env -> String -> Expr -> (Thunk -> IO Value)
mkThunk env x body a = do
  a' <- newIORef a
  eval ((x, a') : env) body

lookupEnv :: Env -> String -> IO (IORef Thunk)
lookupEnv [] s = error $ "Unbound Variable" ++ s
lookupEnv ((x, a) : xs) s =
  if x == s
    then return a
    else lookupEnv xs s

eval :: Env -> Expr -> IO Value
eval env ex =
  case ex of
    EVar n -> do
      th <- lookupEnv env n
      force th
    ELam x e -> return $ VClosure (mkThunk env x e)
    EApp a b -> do
      VClosure c <- eval env a
      c (\() -> eval env b)
    EBool b -> return $ VBool b
    EInt n -> return $ VInt n
    EFix e -> eval env (EApp e (EFix e))

-- Tests
-- -----
-- diverge = fix (\x -> x x)
diverge :: Expr
diverge = EFix (ELam "x" (EApp (EVar "x") (EVar "x")))

-- ignore = \x -> 0
ignore :: Expr
ignore = ELam "x" (EInt 0)

-- omega = (\x -> x x) (\x -> x x)
omega :: Expr
omega =
  EApp
    (ELam "x" (EApp (EVar "x") (EVar "x")))
    (ELam "x" (EApp (EVar "x") (EVar "x")))

-- test1 = (\y -> 42) omega
test1 :: IO Value
test1 = eval [] $ EApp (ELam "y" (EInt 42)) omega

-- test2 = (\y -> 0) diverge
test2 :: IO Value
test2 = eval [] $ EApp ignore diverge
