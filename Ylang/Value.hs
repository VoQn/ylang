{-# LANGUAGE OverloadedStrings #-}
module Ylang.Value where

import qualified Data.Text.Lazy.Builder as LB
import Data.Ratio
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid

import Ylang.Type
import Ylang.Syntax
import Ylang.Display

type Env0 = Map Name Expr

type Env1 = Map Name Val

defaultEnv0 :: Env0
defaultEnv0 = Map.empty

defaultEnv1 :: Env1
defaultEnv1 = Map.empty

data Val
  -- primitives
  = ValBotm           -- _|_
  | ValUnit           -- ()
  | ValKeyw Name      -- :keyword-value
  | ValBool Bool      -- yes / no
  | ValIntn Integer   -- integer number
  | ValFlon Double    -- flonum (floating point number)
  | ValRatn Rational  -- rational number
  | ValChr  Char      -- charactor '0', 'x', '+', ...
  | ValStr  String    -- string
  -- | ValRope Rope    -- rope

  | ValPair Val Val   -- (, v1 v2)
  | ValArray [Val]    -- [v1 v2 v3 ...]

  | ValVar Name Ty Val
  -- function value
  | ValFunc Env1 Expr
  deriving (Eq, Ord, Show)

instance Display Val where
  textBuild v = case v of
    ValBotm -> "_|_"
    ValUnit -> "()"

    ValKeyw k -> ":" <> textBuild k
    ValBool True  -> "yes"
    ValBool False -> "no"
    ValIntn i -> textBuild i
    ValFlon f -> textBuild f
    ValRatn r -> numer r <> "/" <> denom r
      where
      numer = textBuild . numerator
      denom = textBuild . denominator

    ValChr c -> chrLit c
    ValStr  s -> strLit s

    ValPair a b -> parens $ spaceSep $ "," : map textBuild [a,b]
    ValArray vs -> brackets $ spaceSep $ map textBuild vs

    ValVar n t v -> parens $ spaceSep $ "=" : map textBuild [v]
    x -> LB.fromString $ show x

getType :: Val -> Ty
getType x = case x of
  ValBotm   -> TySet
  ValUnit   -> TyUnit
  ValKeyw _ -> TyKeyw
  ValBool _ -> TyBool
  ValIntn _ -> TyIntn
  ValFlon _ -> TyFlon
  ValRatn _ -> TyRatn
  ValChr  _ -> TyChr
  ValStr  _ -> TyStr

  ValPair a b -> TyPair (getType a) (getType b)
  ValArray vs -> TyArray $ map getType vs

  ValVar _ t _ -> t

showType :: Val -> String
showType x = case x of
  ValBotm -> "Any"
  ValUnit -> "Unit"
  ValKeyw _ -> "Keyword"
  ValBool _ -> "Boolean"
  ValIntn _ -> "Integer"
  ValFlon _ -> "Flonum"
  ValRatn _ -> "Rational"
  ValChr _ -> "Charactor"
  ValStr  _ -> "String"
  ValPair v1 v2 -> "(, " ++ showType v1 ++ " " ++ showType v2 ++ ")"

  _ -> "UnknownType"
