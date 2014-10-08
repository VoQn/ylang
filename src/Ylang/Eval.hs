---------------------------------------------------------------------
-- |
-- Module      :  Ylang.Eval
-- Description :  Evaluator for Ylang
-- Copyright   :  (c) 2014 Kazuhiro Mizuhsima
-- License     :  Apache-2
--
-- Maintainer  :  Kazuhiro Mizushima <voqn.tyrantist@gmail.com>
-- Stability   :  unstable
-- Portability :  non-portable (Using -XMultiParamTypeClasses, -XFlexibleInstances)
---------------------------------------------------------------------
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Ylang.Eval where

import Control.Applicative
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State

newtype Eval s e a = Eval { runEval :: s -> (Either e a, s) }

mapEval :: (a -> b) -> Eval s e a -> Eval s e b
mapEval f m = Eval $ \t -> case runEval m t of
  (Left  e, s) -> (Left  e, s)
  (Right x, s) -> (Right (f x), s)

withEval :: (s -> t) -> Eval t e a -> Eval s e a
withEval f m = Eval $ \s -> case runEval m (f s) of
  (Left  e, _) -> (Left  e, s)
  (Right x, _) -> (Right x, s)

evalEval :: Eval s e a -> s -> Either e a
evalEval m = fst . runEval m

execEval :: Eval s e a -> s -> s
execEval m = snd . runEval m

instance Functor (Eval s e) where
  fmap = mapEval

instance Applicative (Eval s e) where
  pure  x = Eval $ \s -> (Right x, s)
  f <*> m = Eval $ \t -> case runEval f t of
    (Left  e, s) -> (Left e, s)
    (Right g, s) -> runEval (mapEval g m) s

instance Monad (Eval s e) where
  return  = pure
  m >>= k = Eval $ \t -> case runEval m t of
    (Left  e, s) -> (Left e, s)
    (Right x, s) -> runEval (k x) s

instance (Error e) => Error (Eval s e a) where
  noMsg      = Eval $ \s -> (Left noMsg, s)
  strMsg msg = Eval $ \s -> (Left (strMsg msg), s)

instance MonadError e (Eval s e) where
  throwError e   = Eval $ \s -> (Left e, s)
  catchError m h = Eval $ \t -> case runEval m t of
    (Left  e, s) -> runEval (h e) s
    (Right x, s) -> (Right x, s)

instance MonadReader s (Eval s e) where
  ask       = Eval $ \s -> (Right s, s)
  local f m = Eval $ runEval m . execEval m . f

instance MonadState s (Eval s e) where
  get   = Eval $ \s -> (Right s, s)
  put s = Eval $ const $ runEval (return ()) s


--
