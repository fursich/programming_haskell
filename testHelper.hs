module TestHelper where

import qualified Language.Haskell.Interpreter as Hint

eval :: String -> String -> Hint.Interpreter()
eval code callerFileName = do
  Hint.loadModules [callerFileName ++ ".hs"]
  Hint.setTopLevelModules ["Main"]
  Hint.setImportsQ [("Prelude", Nothing)]
  a <- Hint.eval code
  Hint.liftIO $ print a

showResult :: String -> String -> IO()
showResult code callerFileName = do
  result <- Hint.runInterpreter (eval code callerFileName)
  case result of
    Left err -> print err
    Right() -> return()

testFor :: String -> String -> IO()
testFor code callerFileName = do
  putStr $ "TEST: " ++ code ++ " = "
  showResult code callerFileName
