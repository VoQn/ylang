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
import Data.Text (Text)
import Ylang.IO
import Ylang.Type

data Lit
  = LitHole          -- ^ hole : _
  | LitUnit          -- ^ unit : ()
  | LitBool Bool     -- ^ boolean  : yes | no
  | LitChr  Char     -- ^ charator : 'a' | 'b' | 'c' | ...
  | LitStr  Text     -- ^ string   : "abc" | "123" | ...
  | LitKey  Text     -- ^ keyword  : :first | :last | ...
  | LitIntn Integer  -- ^ integer  : ... | -1 | 0 | 1 | ...
  | LitFlon Double   -- ^ flonum   : -inf | ... | -0.1 | 0 | 0.1 | ... | +inf
  | LitRatn Rational -- ^ rational : 1/3 | 1/3 | 1/1 | ...
  deriving (Eq, Show)

instance Display Lit where
  buildText LitHole = "_"
  buildText LitUnit = "()"

  buildText (LitBool True)  = "Yes"
  buildText (LitBool False) = "No"

  buildText (LitChr c) = sQuote $ buildText c
  buildText (LitStr s) = dQuote $ buildText s
  buildText (LitKey k) = ":" <> buildText k

  buildText (LitIntn i) = buildText i
  buildText (LitFlon f) = buildText f
  buildText (LitRatn r) = buildText r

typeofLit :: Lit -> Type
typeofLit l = case l of
  LitHole   -> TyBottom
  LitUnit   -> TyUnit
  LitBool _ -> TyBool
  LitChr  _ -> TyChar
  LitStr  _ -> TyString
  LitKey  _ -> TyKeyword
  LitIntn _ -> TyInteger
  LitFlon _ -> TyFlonum
  LitRatn _ -> TyRatio
