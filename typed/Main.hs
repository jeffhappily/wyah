module Main where

-- import AST
import Parser
import Eval
-- import Pretty

import Control.Monad.Trans
import System.Console.Haskeline

process :: String -> IO ()
process line = do
  let res = parseExpr line
  case res of
    Left err -> print err
    Right ex -> do
      case runEval ex of
          Left err -> print err
          Right (ex', t) -> putStrLn $ show ex' <> " : " <> show t

main :: IO ()
main = runInputT defaultSettings loop
  where
  loop = do
    minput <- getInputLine "Arith> "
    case minput of
      Nothing -> outputStrLn "Goodbye."
      Just input -> (liftIO $ process input) >> loop

