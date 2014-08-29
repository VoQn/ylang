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
  | Pair  Expr   Expr
  | Array [Expr]

  -- factor
  | Void
  | Factor [Expr]

  -- anonymous function
  | Func {
      arg1 :: Expr,
      args :: [Expr],
      prem :: [Expr],
      retn :: Expr
    }

  -- redundant
  | Call    Expr [Expr]
  | Arrow   Expr [Expr] Expr
  | Define  Expr [Expr] Expr
  | Declare Expr [Expr] Expr
  deriving (Eq, Ord)

showRatio :: Rational -> String
showRatio x
  = intercalate "/" $ map (show . ($ x)) [numerator, denominator]

currying :: Expr -> Expr
currying f = case f of
  -- atomic value cannot curring
  Void      -> f
  Atom    _ -> f
  Boolean _ -> f
  Int     _ -> f
  Float   _ -> f
  Ratio   _ -> f
  String  _ -> f

  -- collection value cannot curring
  Pair  _ _ -> f
  Array   _ -> f

  -- currring avail with function
  Func _ [] _ _ -> f
  Func i (a:as) es r ->
    currying $ Func i as [] $ Func a [] es r

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
    Pair e1 e2 -> "(, " ++ showl " " (e1:e2:[]) ++ ")"
    Array es -> '[' : showl " " es ++ "]"

    -- factor
    Void -> "()"
    Factor as -> wrapParen $ showl " " as

    Func { arg1 = i, args = as, prem = es, retn = r }
      ->  let
            as' = case as of
              [] -> show i
              _ -> wrapParen $ showl " " (i:as)
            es' = case es of
              [] -> show r
              _  -> wrapParen $ showl " " (es ++ [r])
          in wrapParen $ intercalate " " ["\\" , as', es']

    -- redundant
    Call e1 e2 -> '(' : showl " " (e1:e2) ++ ")"

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
    wrapParen s = '(' : s ++ ")"
