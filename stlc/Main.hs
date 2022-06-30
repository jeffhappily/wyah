module Main where

import Control.Monad.Trans (MonadIO (liftIO))
import Eval (runEval)
import Parser (parseExpr)
import System.Console.Haskeline (
  defaultSettings,
  getInputLine,
  outputStrLn,
  runInputT,
 )
import TypeCheck (runCheck)

process :: String -> IO ()
process line = do
  let res = parseExpr line
  case res of
    Left err -> print err
    Right ex -> do
      case runCheck ex of
        Left tyerr -> print tyerr
        Right _ -> print $ runEval ex

main :: IO ()
main = runInputT defaultSettings loop
  where
    loop = do
      minput <- getInputLine "Stlc> "
      case minput of
        Nothing -> outputStrLn "Goodbye."
        Just input -> liftIO (process input) >> loop
