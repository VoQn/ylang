module Ylang.Value where

import Data.Map as Map

import Ylang.Type
import Ylang.Syntax

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
  | ValChar Char      -- charactor '0', 'x', '+', ...
  | ValStr  String    -- string
  -- | ValRope Rope    -- rope

  | ValPair Val Val   -- (, v1 v2)
  | ValArray [Val]    -- [v1 v2 v3 ...]

  | ValVar Name Ty Val
  -- function value
  | ValFunc Env1 Expr
  deriving (Eq, Ord, Show)
