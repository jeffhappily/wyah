module Eval where

import Control.Monad.Except
import Data.Maybe (fromMaybe)

data Expr
  = Tr
  | Fl
  | IsZero Expr
  | Succ Expr
  | Pred Expr
  | If Expr Expr Expr
  | Zero
  deriving (Eq, Show)

nf :: Expr -> Expr
nf t = fromMaybe t (nf <$> eval1 t)

eval :: Expr -> Maybe Expr
eval t = case isVal (nf t) of
  True  -> Just (nf t)
  False -> Nothing -- term is "stuck"

-- Evaluate a single step.
eval1 :: Expr -> Maybe Expr
eval1 expr = case expr of
  Succ t                    -> Succ <$> (eval1 t)
  Pred Zero                 -> Just Zero
  Pred (Succ t) | isNum t   -> Just t
  Pred t                    -> Pred <$> (eval1 t)
  IsZero Zero               -> Just Tr
  IsZero (Succ t) | isNum t -> Just Fl
  IsZero t                  -> IsZero <$> (eval1 t)
  If Tr  c _                -> Just c
  If Fl _ a                 -> Just a
  If t c a                  -> (\t' -> If t' c a) <$> eval1 t
  _                         -> Nothing

isVal :: Expr -> Bool
isVal Tr = True
isVal Fl = True
isVal t | isNum t = True
isVal _ = False

isNum :: Expr -> Bool
isNum Zero = True
isNum (Pred n) = isNum n
isNum (Succ n) = isNum n
isNum _ = False

data Type
  = TBool
  | TNat
  deriving (Eq)

instance Show Type where
  show (TBool) = "Bool"
  show (TNat) = "Nat"

type Check a = Except TypeError a

data TypeError
  = TypeMismatch Type Type
              -- ^     ^ -- Expected type
              -- |--- Actual Type

instance Show TypeError where
  show (TypeMismatch actual expected) = "Type mismatch: " ++ show actual ++ " is not " ++ show expected

check :: Expr -> Either TypeError Type
check = runExcept . typeof

typeof :: Expr -> Check Type
typeof expr = case expr of
  Succ a -> do
    ta <- typeof a
    case ta of
      TNat -> return TNat
      _    -> throwError $ TypeMismatch ta TNat

  Pred a -> do
    ta <- typeof a
    case ta of
      TNat -> return TNat
      _    -> throwError $ TypeMismatch ta TNat

  IsZero a -> do
    ta <- typeof a
    case ta of
      TNat -> return TBool
      _    -> throwError $ TypeMismatch ta TNat

  If a b c -> do
    ta <- typeof a
    tb <- typeof b
    tc <- typeof c
    if ta /= TBool
    then throwError $ TypeMismatch ta TBool
    else
      if tb /= tc
      then throwError $ TypeMismatch ta tb
      else return tc

  Tr   -> return TBool
  Fl   -> return TBool
  Zero -> return TNat

runEval :: Expr -> Either TypeError (Expr, Type)
runEval ex =
  case check ex of
    Left err -> Left err
    Right t -> case eval ex of
      Nothing -> Left $ TypeMismatch TBool TBool
      Just ex' -> Right (ex', t)
