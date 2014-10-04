---------------------------------------------------------------------
-- |
-- Module      :  Ylang.Syntax.Literal
-- Description :  Literal Sctructure for Ylang
-- Copyright   :  (c) 2014 Kazuhiro Mizuhsima
-- License     :  Apache-2
--
-- Maintainer  :  Kazuhiro Mizushima <voqn.tyrantist@gmail.com>
-- Stability   :  unstable
-- Portability :  non-portable (Using -XOverloadedStrings)
---------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Ylang.Syntax.Literal where

import Data.Monoid
import Ylang.Info
import Ylang.IO

data Lit
  = LitBool Bool     -- ^ boolean  : yes | no
  | LitChr  Char     -- ^ charator : 'a' | 'b' | 'c' | ...
  | LitStr  String   -- ^ string   : "abc" | "123" | ...
  | LitKey  Name     -- ^ keyword  : :first | :last | ...
  | LitIntn Integer  -- ^ integer  : ... | -1 | 0 | 1 | ...
  | LitFlon Double   -- ^ flonum   : -inf | ... | -0.1 | 0 | 0.1 | ... | +inf
  | LitRatn Rational -- ^ rational : 1/3 | 1/3 | 1/1 | ...
  deriving (Eq, Show)

instance Display Lit where
  buildText (LitBool True)  = "Yes"
  buildText (LitBool False) = "No"

  buildText (LitChr c) = "'" <> buildText c <> "'"
  buildText (LitStr s) = "\"" <> buildText s <> "\""
  buildText (LitKey k) = ":" <> buildText k

  buildText (LitIntn i) = buildText i
  buildText (LitFlon f) = buildText f

  buildText _ = undefined
