module Ylang.Type where

data Ty
  = TyBool
  | TyInt
  | TyFunc Ty Ty
  | TyVar Int

instance Show Ty where
  show t = case t of
    TyBool  -> "Bool"
    TyInt   -> "Int"
    TyVar i -> "a" ++ show i
    TyFunc t1 t2 -> show t1 ++ " -> " ++ show t2
