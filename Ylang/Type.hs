module Ylang.Type where

type VName = String

data Ty
  = TySet
  --
  | TyBool

  --
  | TyInt
  | TyRatio
  | TyFloat

  --
  | TyKeyword
  | TyChar
  | TyString

  --
  | TyFunc Ty Ty
  | TyVar VName Int

instance Show Ty where
  show t = case t of
    TySet   -> "Set"

    TyBool  -> "Bool"

    TyInt   -> "Int"
    TyRatio -> "Ratio"
    TyFloat -> "Float"

    TyKeyword -> "Keyword"
    TyChar    -> "Char"
    TyString  -> "String"

    TyVar v i -> v ++ show i
    TyFunc t1 t2 -> "(-> " ++ show t1 ++ " " ++ show t2 ++ ")"
