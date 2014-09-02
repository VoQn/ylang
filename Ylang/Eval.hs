module Ylang.Eval where

import Data.List (intercalate)
import qualified Data.Map as Map

import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

import Ylang.Syntax
import Ylang.Value
import Ylang.Display

type Eval a
  = ReaderT Env (ErrorT String
                (WriterT [String] (StateT Env Identity))) a

type Result a b = ((Either String a, [String]), b)

getResult :: Result a b -> Either String a
getResult = fst . fst

getEnv :: Result a b -> b
getEnv = snd

runEval :: Env -> Eval a -> Result a Env
runEval env evl =
  runIdentity (runStateT (runWriterT (runErrorT (runReaderT evl env))) env)

defaultEnv :: Env
defaultEnv = Map.empty

defPref :: String
defPref = "def_"

decPref :: String
decPref = "dec_"

eval :: Expr -> Eval Expr
  -- atomic
eval v@(Void     ) = return v
eval k@(Keyword _) = return k
eval b@(Boolean _) = return b
eval i@(Int     _) = return i
eval f@(Float   _) = return f
eval r@(Ratio   _) = return r
eval c@(Char    _) = return c
eval s@(String  _) = return s

  -- collection
eval (Pair e1 e2) = do
  r1 <- eval e1
  r2 <- eval e2
  return $ Pair r1 r2

eval (Array es) =
  mapM eval es >>= return . Array

eval (Atom n) = do
  tell [n]
  env <- get
  let key = defPref ++ n
  case Map.lookup key env of
    Just x -> return x
    Nothing ->
      throwError ("<Undefined Value> : " ++ n)

eval f@(Func _ _ _ _) = return f

eval d@(Declare n pm as r) = do
  env <- get
  let key = decPref ++ n
  case Map.lookup key env of
    Just (Declare _ q b l) ->
      let messages = [
              "<Conflict Definition>",
              "Already Defined ::",
              toString (Declare n q b l),
              "But Reassigned ::",
              toString (Declare n pm as r)
            ]
      in throwError $ intercalate " " messages
    Nothing -> do
      put $ Map.insert key d env
      return $ d

eval d@(Define n v) = do
  env <- get
  let key = defPref ++ n
  case Map.lookup key env of

    -- already assigned
    Just x ->
      let
        messages = [
            "<Conflict Definition>",
            "Already Defined ::",
            toString (Define n x),
            "But Reassigned ::",
            toString (Define n v)
          ]
        message = intercalate " " messages
      in throwError message

  -- can assign
    Nothing -> do
      v' <- eval v
      put $ Map.insert key v' env
      return $ d

  -- identity
eval (Call e []) = return e

eval _ = undefined
