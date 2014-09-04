module Main where

import Control.Monad.Trans

import System.IO
import System.Environment
import System.Console.Haskeline

import Ylang.Syntax
import Ylang.Parser
import Ylang.Value
import Ylang.Eval
import Ylang.Display as D

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> repl
    [fname] -> processFile fname >> return ()

repl :: IO ()
repl = do
  runInputT defaultSettings (loop initEnv)
  where
    loop env = do
      minput <- getInputLine "ylsh> "
      case minput of
        Nothing    -> outputStrLn "Goodbye."
        Just input -> do
          env' <- liftIO $ process env input
          case env' of
            Just envn -> loop envn
            Nothing   -> loop env

initEnv :: Env1
initEnv = defaultEnv1

process :: Env1 -> String -> IO (Maybe Env1)
process env line = do
  case parseTopLevel line of
    Left  err  -> print err >> return Nothing
    Right exprs -> do
      envn <- evaluate env exprs
      return $ Just envn

processFile :: String -> IO (Maybe Env1)
processFile fname = readFile fname >>= process initEnv

evaluate :: Env1 -> [Expr] -> IO Env1
evaluate env exs = do
  let exec = runEval1 env $ mapM eval1 exs
  res <- getResult1 exec
  case res of
    Left  err -> liftIO $ print err
    Right val -> liftIO $ mapM_ D.display val
  getEnv1 exec
