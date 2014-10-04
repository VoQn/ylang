{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Ylang.IO where

import Control.Arrow
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as LB
import qualified Data.Text.Lazy.IO as LIO
import Data.Monoid

class Display a where
  buildText :: a -> Builder

  display :: a -> IO ()
  display = buildText >>> LB.toLazyText >>> LIO.putStrLn

instance Display Char where
  buildText = LB.singleton

instance Display String where
  buildText = LB.fromString

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

parens :: Builder -> Builder
parens = ("(" <>) >>> (<> ")")

sep :: (Monoid m) => m -> [m] -> m
sep _ []     = mempty
sep _ (m:[]) = m
sep j (m:ms) = m <> j <> sep j ms

spaces :: [Builder] -> Builder
spaces = sep " "
