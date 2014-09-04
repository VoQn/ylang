{-# LANGUAGE LambdaCase #-}
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
main = getArgs >>= \case
  []      -> repl
  [fname] -> processFile fname >> return ()

repl :: IO ()
repl = runInputT defaultSettings $ loop initEnv
  where
  loop env = getInputLine "ylsh> " >>= \case
    Nothing    -> outputStrLn "Goodbye."
    Just input -> liftIO (process env input) >>= \case
      Just newEnv -> loop newEnv
      Nothing     -> loop env

initEnv :: Env1
initEnv = defaultEnv1

process :: Env1 -> String -> IO (Maybe Env1)
process env line = case parseTopLevel line of
  Left  err   -> print err          >>  return Nothing
  Right exprs -> evaluate env exprs >>= return . Just

processFile :: String -> IO (Maybe Env1)
processFile fname = readFile fname >>= process initEnv

evaluate :: Env1 -> [Expr] -> IO Env1
evaluate env exs = do
  let exec = runEval1 env $ mapM eval1 exs
  getResult1 exec >>= \case
    Left  err -> liftIO $ print err
    Right val -> liftIO $ mapM_ D.display val
  getEnv1 exec
