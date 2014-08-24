module Main where

import Control.Monad.Trans

import System.IO
import System.Environment
import System.Console.Haskeline

import Ylang.Parser

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> repl
    [fname] -> processFile fname >> return ()

repl :: IO ()
repl = runInputT defaultSettings loop
  where
    loop = do
      minput <- getInputLine "ylsh> "
      case minput of
        Nothing    -> outputStrLn "Goodbye."
        Just input -> (liftIO $ process input) >> loop

process :: String -> IO ()
process line = do
  let res = parseTopLevel line
  case res of
    Left  err  -> print err
    Right expr -> mapM_ print expr

processFile :: String -> IO ()
processFile fname = readFile fname >>= process
