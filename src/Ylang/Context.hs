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
{-# LANGUAGE LambdaCase #-}
module Ylang.Context where

import Data.Map (Map)
import Control.Monad
import Control.Monad.Error
import Control.Monad.Reader
import Ylang.Info
import Ylang.Type
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

pushContext :: (MonadReader (Env a) m) => Name -> Binding -> m (Env a)
pushContext n b = do
  env <- ask
  let ctx = (n, b) : context env
  return $ env { context = ctx }

pushNameBind :: (MonadReader (Env a) m) => Name -> m (Env a)
pushNameBind n = pushContext n NameBind

pushVarBind :: (MonadReader (Env a) m) => Name -> Type -> m (Env a)
pushVarBind n = pushContext n . VarBind

getBind :: (MonadReader (Env a) m, MonadError RuntimeError m) => Info -> Int -> m (Name, Binding)
getBind info idx = do
  env <- ask
  let ctx = context env
  let len = length ctx
  if idx < len
    then return $ ctx !! idx
    else throwError $ OutOfIndex info idx len

getBinding :: (MonadReader (Env a) m, MonadError RuntimeError m) => Info -> Int -> m Binding
getBinding info = liftM snd . getBind info

getBindName :: (MonadReader (Env a) m, MonadError RuntimeError m) => Info -> Int -> m Name
getBindName info = liftM fst . getBind info

nameToIndex :: (MonadReader (Env a) m, MonadError RuntimeError m) => Info -> Name -> m Int
nameToIndex info x = ask >>= search 0 . context
  where
  search c = \case
    [] -> throwError $ UnboundSymbol info x
    ((y, NameBind) : ctx')
      | x == y    -> return c
      | otherwise -> search (c + 1) ctx'
    (_ : ctx') -> search (c + 1) ctx'

nameToIndex' :: Context -> Info -> Name -> Either RuntimeError Int
nameToIndex' ctx info x = search 0 ctx
  where
  search c = \case
    [] -> Left $ UnboundSymbol info x
    ((y, VarBind _) : ctx')
      | x == y    -> Right c
      | otherwise -> search (c + 1) ctx'
    (_ : ctx')    -> search (c + 1) ctx'
