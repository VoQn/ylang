{-# LANGUAGE OverloadedStrings #-}
module Ylang.Info where

import           Data.Monoid
import           Ylang.IO

type Name = String

data Info
  = Unknown
  | FileInput { fileName :: Name, line :: Int, column :: Int }
  deriving (Eq, Show)

instance Display Info where
  buildText Unknown = "#unknown#"

  buildText FileInput { fileName = n, line = l, column = c } =
    buildText n <> " " <> sep "," (map buildText [l, c])
