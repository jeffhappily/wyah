module TypeCheck where

import Control.Monad.Except
import Control.Monad.Reader

import Syntax
import Type

data TypeError
  = Mismatch Type Type
  | NotFunction Type
  | NotInScope Name

instance Show TypeError where
  show (Mismatch actual expected) = "Couldn't match expected type '" <> show expected <> "' with actual type: '" <> show actual <> "'"
  show (NotFunction t) = "Tried to apply to non-function type: " <> show t
  show (NotInScope x) = "Variable not in scope: " <> show x

type Check = ExceptT TypeError (Reader Env)

type Env = [(Name, Type)]

extend :: (Name, Type) -> Env -> Env
extend xt env = xt : env

inEnv :: (Name, Type) -> Check a -> Check a
inEnv (x,t) = local (extend (x,t))

lookupVar :: Name -> Check Type
lookupVar x = do
  env <- ask
  case lookup x env of
    Just e  -> return e
    Nothing -> throwError $ NotInScope x


check :: Expr -> Check Type
check expr = case expr of
  Var a -> lookupVar a

  Lit (LInt _) -> return $ TInt

  Lit (LBool _) -> return $ TBool
  
  App e1 e2 -> do
    t1 <- check e1
    t2 <- check e2

    case t1 of
      TArr inp out ->
        if t2 == inp
          then return out
          else throwError $ Mismatch t2 inp
      _ -> throwError $ NotFunction t1

  Lam x t e -> do
    tout <- inEnv (x, t) $ check e
    return $ TArr t tout

  If cl e1 e2 -> do
    tc <- check cl
    t1 <- check e1
    t2 <- check e2

    case tc of
      TBool ->
        if t1 == t2 then return t1 else throwError $ Mismatch t2 t1
      _ -> throwError $ Mismatch tc TBool

runCheck :: Expr -> Either TypeError Type
runCheck expr = flip runReader [] $ runExceptT (check expr)
