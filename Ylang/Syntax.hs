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
  | Define   Name    [Expr]  Expr
  | Declare  Name    [Expr]  Expr
  deriving (Eq, Ord)

instance Show Expr where
  show expr = case expr of
    Var    name -> name
    Operator op -> op
    Int     num -> show num
    Float   num -> show num
    String  str -> '"' : str ++ "\""
    Boolean b | b -> "#t" | otherwise -> "#f"

    List es -> '[' : showl es ++ "]"

    Call e1 []
      -> '(' : show e1 ++ ")"
    Call e1 e2
      -> '(' : show e1 ++ " " ++ showl e2 ++ ")"

    Lambda as e -> case as of
      [a]
        -> '(' : show a ++ " -> " ++ show e ++ ")"
      _
        -> "((" ++ showl as ++ ") -> " ++ show e ++ ")"

    Define n as e -> case as of
      []
        -> '(' : n ++ " = " ++ show e ++ ")"
      _
        -> "((" ++ n ++ " " ++ showl as ++ ") = " ++ show e ++ ")"

    Declare n as e -> case as of
      []
        -> '(' : n ++ " : " ++ show e ++ ")"
      [a]
        -> '(' : n ++ " : (" ++ show a ++ " -> " ++ show e ++ "))"
      _
        -> '(' : n ++ " : ((" ++ (intercalate " -> " $ map show as) ++ ") -> " ++ show e ++ "))"

    where
    showl = intercalate " " . map show
