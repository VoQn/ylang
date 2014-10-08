
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Ylang.Context where

import Data.Map (Map)
import Data.Monoid
import Control.Monad
import Control.Monad.Error
import Control.Monad.Reader
import Ylang.IO
import Ylang.Info
import Ylang.Type
import Ylang.Eval

import qualified Data.Map as Map

data RuntimeError
  = UnboundSymbol Info Name
  | OutOfIndex    Info Int Int
  deriving (Eq, Show)

instance Display RuntimeError where
  buildText err = case err of
    UnboundSymbol info name ->
      buildText info <> "\n\t" <>
      "Unbound Symbol: " <> buildText name

    OutOfIndex info i l ->
      buildText info <> "\n\t" <>
      "Out of range index\n\t\t" <>
      "Target depth: " <> buildText l <> ", but indexed: " <> buildText i

data Binding
  = NameBind      -- ^ λx.x
  | VarBind  Type -- ^ λx:T
  | TypeBind
  deriving (Eq, Show)

type Context = [(Name, Binding)]

data Env a = Env {
    symbols :: Map Name a,
    context :: Context
  }
  deriving (Eq, Show)

initEnv :: Env a
initEnv = Env { symbols = Map.empty, context = [] }

pushContext :: Name -> Binding -> Eval (Env a) e (Env a)
pushContext n b = do
  env <- ask
  let ctx = (n, b) : context env
  return $ env { context = ctx }

pushNameBind :: Name -> Eval (Env a) e (Env a)
pushNameBind n = pushContext n NameBind

pushVarBind :: Name -> Type -> Eval (Env a) e (Env a)
pushVarBind n = pushContext n . VarBind

getBind :: Info -> Int -> Eval (Env a) RuntimeError (Name, Binding)
getBind info idx = do
  env <- ask
  let ctx = context env
  let len = length ctx
  if idx < len
    then return $ ctx !! idx
    else throwError $ OutOfIndex info idx len

getBinding :: Info -> Int -> Eval (Env a) RuntimeError Binding
getBinding info = liftM snd . getBind info

getBoundName :: Info -> Int -> Eval (Env a) RuntimeError Name
getBoundName info = liftM fst . getBind info

nameToIndex :: Info -> Name -> Eval (Env a) RuntimeError Int
nameToIndex info x = ask >>= search 0 . context
  where
  search c ctx = case ctx of
    [] -> throwError $ UnboundSymbol info x
    ((y, NameBind) : ctx')
      | x == y    -> return c
      | otherwise -> search (c + 1) ctx'
    (_ : ctx') -> search (c + 1) ctx'
--
