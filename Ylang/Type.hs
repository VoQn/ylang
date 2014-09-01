{-# LANGUAGE OverloadedStrings #-}
module Ylang.Type where

import Data.Monoid ((<>))
import Ylang.Syntax
import Ylang.Display

data Ty
  = TyUnit  -- ()
  | TySet   --
  --
  | TyBool  -- boolean (yes / no)

  -- Numbers
  | TyIntn  -- integer number type
  | TyRatn  -- rational number type
  | TyFlon  -- floating point number type

  -- Letter
  | TyKeyw  -- keyword type
  | TyChar  -- charactor type
  | TyStr   -- string
  | TyRope

  --
  | TyFunc Ty Ty -- function
  | TyVar Name Int -- type variable
  deriving (Eq, Ord, Show)

instance Display Ty where
  textBuild t = case t of
    TyUnit -> "()"
    TySet  -> "Set"

    TyBool -> "Bool"

    TyIntn -> "Integer"
    TyRatn -> "Ratio"
    TyFlon -> "Flonum"

    TyKeyw -> "Keyword"
    TyChar -> "Char"
    TyStr　-> "String"
    TyRope -> "Rope"

    TyVar v i -> textBuild v <> textBuild i
    TyFunc t1 t2 -> "(-> " <> textBuild t1 <> " " <> textBuild t2 <> ")"
