module Ylang.Syntax where

import Data.List (intercalate)

type Name = String

data Expr
  = Var      Name
  | Operator Name
  | List     [Expr]
  | Int      Integer
  | Float    Double
  | String   String
  | Boolean  Bool
  | Call     Expr    [Expr]
  | Lambda   [Expr]  Expr
  | Arrow    [Expr]  Expr
  | Define   Expr    [Expr]  Expr
  | Declare  Expr    [Expr]  Expr
  deriving (Eq, Ord)

instance Show Expr where
  show expr = case expr of
    Var    name -> name
    Operator op -> op
    Int     num -> show num
    Float   num -> show num
    String  str -> '"' : str ++ "\""
    Boolean b | b -> "#t" | otherwise -> "#f"

    List es -> '[' : showl " " es ++ "]"

    Call e1 e2 -> case e2 of
      [] -> '(' : show e1 ++ ")"
      _  -> '(' : show e1 ++ " " ++ showl " " e2 ++ ")"

    Lambda as e
      -> "((\\ " ++ showl " " as ++ ") " ++ show e ++ ")"

    Arrow as r -> case as of
      [] -> "(-> () " ++ show r ++ ")"
      _  -> "(-> " ++ showl " " as ++ " " ++ show r ++ ")"

    Define n as e -> case as of
      [] -> "(= " ++ show n ++ " " ++ show e ++ ")"
      _  -> "(= (" ++ show n ++ " " ++ showl " " as ++ ") " ++ show e ++ ")"

    Declare n as e -> case as of
      [] -> "(: " ++ show n ++ " " ++ show e ++ ")"
      _  -> "(: (" ++ show n ++ " " ++ showl " " as ++ ") " ++ show e ++ ")"

    where
    showl s = intercalate s . map show
