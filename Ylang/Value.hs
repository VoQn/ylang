module Ylang.Value where

import Ylang.Syntax

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

  -- function value
  | ValFunc
  deriving (Eq, Ord, Show)
