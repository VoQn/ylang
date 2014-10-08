---------------------------------------------------------------------
-- |
-- Module      :  Ylang.Error
-- Description :  Error for Ylang
-- Copyright   :  (c) 2014 Kazuhiro Mizuhsima
-- License     :  Apache-2
--
-- Maintainer  :  Kazuhiro Mizushima <voqn.tyrantist@gmail.com>
-- Stability   :  unstable
-- Portability :  non-portable (Using -XOverloadedStrings)
---------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Ylang.Error where

import Data.Monoid
import Ylang.IO
import Ylang.Info

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
