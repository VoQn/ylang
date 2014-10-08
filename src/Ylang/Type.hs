---------------------------------------------------------------------
-- |
-- Module      :  Ylang.Type
-- Description :  Type Structure for Ylang
-- Copyright   :  (c) 2014 Kazuhiro Mizuhsima
-- License     :  Apache-2
--
-- Maintainer  :  Kazuhiro Mizushima <voqn.tyrantist@gmail.com>
-- Stability   :  unstable
-- Portability :  non-portable (Using -XOverloadedStrings)
---------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Ylang.Type
  ( module Ylang.Type.Kind
  , Type(..)
  , kindOfType
  ) where

import Data.Monoid
import Ylang.IO
import Ylang.Info
import Ylang.Type.Kind

data Type
  = TyTop                  -- ^ Top type
  | TyBottom               -- ^ Bottom type
  | TyUnit                 -- ^ Unit type ()

  -- Primitive Types
  | TyBool                 -- ^ Boolean
  | TyChar                 -- ^ Charactor
  | TyString               -- ^ String
  | TyKeyword              -- ^ Keyword
  | TyNatural              -- ^ Natural Number
  | TyInteger              -- ^ Integer Number
  | TyFlonum               -- ^ Floating Point Number
  | TyRatio                -- ^ Rational Number

  -- Structural, Contextual
  | TyVar   Int  Int       -- ^ type-variable
  | TyArrow Type Type      -- ^ T -> T
  | TyAll   Name Type      -- ^ ∀X<:T.T
  | TyAbs   Name Kind Type -- ^ λX::K.T
  | TyApp   Type Type      -- ^ T T
  deriving (Eq, Show)

kindOfType :: Type -> Kind
kindOfType (TyArrow t1 t2) = KindArrow (kindOfType t1) (kindOfType t2)
kindOfType (TyAll _ t) = kindOfType t
kindOfType (TyAbs _ k _) = k
kindOfType _ = KindStar

instance Display Type where
  buildText TyTop     = "Set"
  buildText TyBottom  = "_|_"
  buildText TyUnit    = "()"

  buildText TyBool    = "Bool"
  buildText TyChar    = "Char"
  buildText TyString  = "String"
  buildText TyKeyword = "Keyword"
  buildText TyNatural = "Nat"
  buildText TyInteger = "Integer"
  buildText TyFlonum  = "Flonum"
  buildText TyRatio   = "Rational"

  buildText TyVar{}         = undefined
  buildText (TyArrow t1 t2) = buildText t1 <> " -> " <> buildText t2
  buildText (TyAll n t)     = "forall " <> buildText n <> "." <> buildText t
  buildText TyAbs{}         = undefined
  buildText (TyApp t1 t2)   = spaces $ map buildText [t1, t2]
