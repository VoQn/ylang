module Ylang.Type where

import Ylang.Syntax

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

instance Show Ty where
  show t = case t of
    TySet  -> "Set"

    TyBool -> "Bool"

    TyIntn -> "Integer"
    TyRatn -> "Ratio"
    TyFlon -> "Flonum"

    TyKeyw -> "Keyword"
    TyChar -> "Char"
    TyStr　-> "String"
    TyRope -> "Rope"

    TyVar v i -> v ++ show i
    TyFunc t1 t2 -> "(-> " ++ show t1 ++ " " ++ show t2 ++ ")"
