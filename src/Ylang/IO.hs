---------------------------------------------------------------------
-- |
-- Module      :  Ylang.IO
-- Description :  IO Instruction for Ylang
-- Copyright   :  (c) 2014 Kazuhiro Mizuhsima
-- License     :  Apache-2
--
-- Maintainer  :  Kazuhiro Mizushima <voqn.tyrantist@gmail.com>
-- Stability   :  unstable
-- Portability :  non-portable (Using -XOverloadedStrings, -XFlexibleInstances)
---------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Ylang.IO where

import Control.Arrow
import Data.Monoid
import Data.Ratio
import Data.Text              (Text)
import Data.Text.Lazy.Builder (Builder)

import qualified Data.Text.Lazy.Builder as LB
import qualified Data.Text.Lazy.IO      as LIO

class Display a where
  buildText :: a -> Builder

  display :: a -> IO ()
  display = buildText >>> LB.toLazyText >>> LIO.putStrLn

instance Display Char where
  buildText = LB.singleton

instance Display String where
  buildText = LB.fromString

instance Display Text where
  buildText = LB.fromText

instance Display Builder where
  buildText = id

fromShow :: (Show a) => a -> Builder
fromShow = show >>> buildText

instance Display Int where
  buildText = fromShow

instance Display Integer where
  buildText = fromShow

instance Display Double where
  buildText = fromShow

instance Display Rational where
  buildText x =
    sep "/" $ map buildText [numerator x, denominator x]

close :: (Monoid m) => m -> m -> m -> m
close o c x = o <> x <> c

sep :: (Monoid m) => m -> [m] -> m
sep _ []     = mempty
sep _ (m:[]) = m
sep j (m:ms) = m <> j <> sep j ms

parens :: Builder -> Builder
parens = close "(" ")"

sQuote :: Builder -> Builder
sQuote = close "'" "'"

dQuote :: Builder -> Builder
dQuote = close "\"" "\""

spaces :: [Builder] -> Builder
spaces = sep " "
