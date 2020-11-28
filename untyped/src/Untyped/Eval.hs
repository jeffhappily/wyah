module Untyped.Eval (
  runEval
) where

import Untyped.AST

import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Map as Map

data Value
  = VInt Integer
  | VBool Bool
  | VClosure String Expr Scope

instance Show Value where
  show (VInt x) = show x
  show (VBool x) = show x
  show (VClosure a b env) = "\\" <> a <> " . " <> printExprWithScope b env

printExprWithScope :: Expr -> Scope -> String
printExprWithScope (Var x) env = maybe x show $ env Map.!? x
printExprWithScope (App a b) env = "(" <> printExprWithScope a env <> " " <> printExprWithScope b env <> ")"
printExprWithScope (Lam a b) env = "(" <> "\\" <> a <> " . " <> printExprWithScope b env <> ")"
printExprWithScope (Lit l) _ = show l

newtype EvalState = EvalState
  { depth :: Int
  } deriving (Show)

-- | Run evaluation after increasing the depth, then decrease it afterwards
inc :: Eval a -> Eval a
inc m = do
  modify $ \s -> s { depth = depth s + 1 }
  out <- m
  modify $ \s -> s { depth = depth s - 1 }
  return out

-- | Create @Step@ and push to @Eval@ state
red :: Expr -> Eval ()
red x = do
  d <- gets depth
  tell [(d, x)]
  return ()

-- | Each step being evaluated
type Step = (Int, Expr)

-- | Evaluation monad transformer
type Eval a = WriterT [Step] (State EvalState) a

-- | Environment
type Scope = Map.Map String Value

eval :: Scope -> Expr -> Eval Value
eval env expr = case expr of

  Lit (LInt x) -> do
    return $ VInt (fromIntegral x)

  Var x -> do
    red expr
    return $ env Map.! x

  Lam x body -> inc $ do
    return (VClosure x body env)

  App a b -> inc $ do
    x <- eval env a
    red a
    y <- eval env b
    red b
    apply x y

extend :: Scope -> String -> Value -> Scope
extend env v t = Map.insert v t env

apply :: Value -> Value -> Eval Value
apply (VClosure n e clo) ex = do
  eval (extend clo n ex) e
apply _ _  = error "Tried to apply non-closure"

emptyScope :: Scope
emptyScope = Map.empty

runEval :: Expr -> (Value, [Step])
runEval x = evalState (runWriterT (eval emptyScope x)) (EvalState 0)
