---------------------------------------------------------------------
-- |
-- Module      :  Ylang.Context
-- Description :  Context Structure for Ylang
-- Copyright   :  (c) 2014 Kazuhiro Mizuhsima
-- License     :  Apache-2
--
-- Maintainer  :  Kazuhiro Mizushima <voqn.tyrantist@gmail.com>
-- Stability   :  unstable
-- Portability :  non-portable (Using -XOverloadedStrings, -XFlexibleInstances)
---------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Ylang.Context where

import Data.Map (Map)
import Control.Monad
import Control.Monad.Error
import Control.Monad.Reader
import Ylang.Info
import Ylang.Type
import Ylang.Eval
import Ylang.Error

import qualified Data.Map as Map

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
