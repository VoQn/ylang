module Ylang.Syntax where

import Data.Ratio (numerator, denominator)
import Data.List (intercalate)

type Name = String

data Expr
  = Var      Name
  | Operator Name
  | List     [Expr]
  | Int      Integer
  | Float    Double
  | Ratio    Rational
  | String   String
  | Boolean  Bool
  | Call     Expr    [Expr]
  | Lambda   [Expr]  Expr
  | Arrow    Expr    [Expr]  Expr
  | Define   Expr    [Expr]  Expr
  | Declare  Expr    [Expr]  Expr
  deriving (Eq, Ord)

showRatio :: Rational -> String
showRatio x
  = intercalate "/" $ map (show . ($ x)) [numerator, denominator]

instance Show Expr where
  show expr = case expr of
    Var    name -> name
    Operator op -> op
    Int     num -> show num
    Float   num -> show num
    Ratio   num -> showRatio num
    String  str -> '"' : str ++ "\""
    Boolean b | b -> "yes" | otherwise -> "no"

    List es -> '[' : showl " " es ++ "]"
    Call e1 e2 -> '(' : showl " " (e1:e2) ++ ")"

    Lambda as e
      -> "((\\ " ++ showl " " as ++ ") " ++ show e ++ ")"

    Arrow i as r
      -> "(-> " ++ showl " " (i : as ++ [r]) ++ ")"

    Define n as e -> case as of
      [] -> "(= "  ++ showl " " (n:[e]) ++ ")"
      _  -> "(= (" ++ showl " " (n:as)  ++ ") " ++ show e ++ ")"

    Declare n as e -> case as of
      [] -> "(: "  ++ showl " " (n:[e]) ++ ")"
      _  -> "(: (" ++ showl " " (n:as)  ++ ") " ++ show e ++ ")"

    where
    showl s = intercalate s . map show
