module Ylang.Type.Kind where

data Kind
  = KindStar            -- ^ *
  | KindArrow Kind Kind -- ^ * => *
  deriving (Eq, Show)
