module Syntax where

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
  deriving (Eq, Ord, Show)
