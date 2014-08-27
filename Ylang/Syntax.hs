module Ylang.Syntax where

import Data.Ratio (numerator, denominator)
import Data.List (intercalate)

type Name = String

data Expr
  -- atomic
  = Atom    Name
  | Int     Integer
  | Float   Double
  | Ratio   Rational
  | String  String
  | Boolean Bool

  -- collection
  | Array    [Expr]

  -- factor
  | Call    Expr [Expr]
  | Lambda  Expr [Expr] Expr

  -- redundant
  | Arrow   Expr [Expr] Expr
  | Define  Expr [Expr] Expr
  | Declare Expr [Expr] Expr
  deriving (Eq, Ord)

showRatio :: Rational -> String
showRatio x
  = intercalate "/" $ map (show . ($ x)) [numerator, denominator]

instance Show Expr where
  show expr = case expr of
    -- atomic
    Atom    s -> s
    Int     n -> show n
    Float   n -> show n
    Ratio   n -> showRatio n
    String  s -> '"' : s ++ "\""
    Boolean b | b -> "yes" | otherwise -> "no"

    -- collection
    Array es -> '[' : showl " " es ++ "]"

    -- factor
    Call e1 e2 -> '(' : showl " " (e1:e2) ++ ")"

    Lambda i as e -> case as of
      [] -> "(-> " ++ show i ++ " " ++ show e ++ ")"
      _  -> "(-> (" ++ showl " " (i:as) ++ ") " ++ show e ++ ")"

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
