module Ylang.Syntax.Term where

import Ylang.Info
import Ylang.Type

import Ylang.Syntax.Literal

data Term
  = TmLit   Info Lit            -- ^ literal
  | TmSym   Info Name           -- ^ symbol
  | TmVar   Info Int  Int       -- ^ λ 1     (no-name-variable-expression)
  | TmAbs   Info Name Type Term -- ^ λx:T.t  (lambda abstruction)
  | TmApp   Info Term Term      -- ^ f x     (apply abstruction to term)
  | TmTyAbs Info Name Type Term -- ^ λX<:T.t (type abstruction)
  | TmTyApp Info Term [Type]    -- ^ t [T]   (apply types to term)
  deriving (Eq, Show)
{-
data Context
  = CtxEmpty               -- ^ ∅    (empty context)
  | CtxBindTerm Term Type  -- ^ λx.T (bind type to term)
  | CtxBindType Type Type  -- ^ λX.T (bind type to type)
  deriving (Eq, Show)
-}
data ValAbs
  = ValAbsTerm Name Type Term -- ^ λx:T.t
  | ValAbsType Name Type Term -- ^ λX<:T.t
  deriving (Eq, Show)

data Value
  = ValBotm          -- ^ _|_ (well-known as undefined value)
  | ValUnit          -- ^ ()  (well-known as void value)
  | ValBool Bool     -- ^ true | false
  | ValIntn Integer  -- ^ integer number
  | ValFlon Double   -- ^ floating point number
  | ValRatn Rational -- ^ rational number
  | ValChar Char     -- ^ charactor
  | ValStr  String   -- ^ string
  | ValKey  Name     -- ^ keyword
  deriving (Eq, Show)
