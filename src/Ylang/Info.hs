module Ylang.Info where

type Name = String

data Info
  = Unknown
  | FileImput { fileName :: Name, line :: Int, column :: Int }
  deriving (Eq, Show)
