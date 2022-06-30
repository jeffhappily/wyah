module Eval where

import qualified Data.Map as Map

import Syntax (Expr (..), Lit (LBool, LInt))

data Value
  = VInt Int
  | VBool Bool
  | VClosure String Expr Eval.Scope

instance Show Value where
  show (VInt x) = show x
  show (VBool x) = show x
  show VClosure {} = "<closure>"

type Scope = Map.Map String Value

extendScope :: (String, Value) -> Scope -> Scope
extendScope (k, v) = Map.insert k v

lookupScope :: String -> Scope -> Value -- should use Maybe
lookupScope = flip (Map.!)

eval :: Scope -> Expr -> Value
eval env expr = case expr of
  Var n -> lookupScope n env
  Lit (LInt n) -> VInt n
  Lit (LBool b) -> VBool b
  Lam x _ e -> VClosure x e env
  App e1 e2 ->
    let f = eval env e1
        a = eval env e2
     in apply f a
  If cl e1 e2 ->
    let b = eval env cl
        v1 = eval env e1
        v2 = eval env e2
     in case b of
          (VBool b') -> if b' then v1 else v2
          -- won't happen because we type-checked
          _ -> error "Error type"

apply :: Value -> Value -> Value
apply (VClosure x e env) a = eval (extendScope (x, a) env) e
apply _ _ = error "Tried to non closure"

runEval :: Expr -> Value
runEval = eval Map.empty
