module Ylang.Eval where

import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State

import Ylang.Syntax

type Env = Map.Map Name Expr
type Eval a = ReaderT Env (ErrorT String Identity) a

runEval :: Env -> Eval a -> Either String a
runEval env evl = runIdentity (runErrorT (runReaderT evl env))

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
    env <- ask
    r1 <- eval e1
    r2 <- eval e2
    return $ Pair r1 r2
{-
  Array es ->
    let
      rs = do
        e <- es
        r <- return $ eval env e
        return $ r
    in return $ Array rs
-}
  -- factor
  Define n v -> do
    env <- ask
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
        let env' = Map.insert n v' env
        eval (Atom n)

  -- void
  Factor []     -> return expr

  -- identity
  Factor (e:[]) -> return e

  -- applycation
  Factor (f:args) -> undefined

  _ -> return expr

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
