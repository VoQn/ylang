module Ylang.Eval where

import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

import Ylang.Syntax

type Env = Map.Map Name Expr

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

eval :: Expr -> Eval Expr
eval expr = case expr of
  -- atomic
  Boolean _ -> return expr
  Int     _ -> return expr
  Float   _ -> return expr
  Ratio   _ -> return expr
  String  _ -> return expr

  -- collection
  Pair e1 e2 -> do
    r1 <- eval e1
    r2 <- eval e2
    return $ Pair r1 r2

  Array es -> do
    rs <- mapM eval es
    return $ Array rs

  Atom n -> do
    tell [n]
    env <- get
    case Map.lookup n env of
      Just x -> return x
      Nothing ->
        throwError ("<Undefined Value> : " ++ n)

  f@(Func _ _ _ _) -> return expr

  -- factor
  Define n v -> tryAssign n v

  -- void
  Factor []     -> return expr

  -- identity
  Factor (e:[]) -> return e

  -- applycation
  Factor (f:args) -> undefined

  _ -> return expr

tryAssign :: Name -> Expr -> Eval Expr
tryAssign n v = do
  env <- get
  case Map.lookup n env of

    -- already assigned
    Just x ->
      let
        messages = [
            "<Conflict Definition>",
            "Already Defined ::",
            show (Define n x),
            "But Reassigned ::",
            show (Define n v)
          ]
        message = intercalate " " messages
      in throwError message

    -- can assign
    Nothing -> do
      v' <- eval v
      put $ Map.insert n v' env
      return $ Atom n

declaration :: Env -> [Expr] -> (Env, Expr)
declaration = undefined

-- anonymous :: Env -> Expr -> [Expr] -> (Env, Expr)
anonymous e vs es r as
  | length vs == length as = -- apply and return
      undefined

  | length vs > length as = -- partial apply
      undefined

  | otherwise = -- arity over
      undefined
