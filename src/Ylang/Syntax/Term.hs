module Ylang.Syntax.Term where

import Ylang.Info
import Ylang.Type

data Lit
  = LitBool Bool     -- ^ boolean  : yes | no
  | LitChar Char     -- ^ charator : 'a' | 'b' | 'c' | ...
  | LitStr  String   -- ^ string   : "abc" | "123" | ...
  | LitKey  Name     -- ^ keyword  : :first | :last | ...
  | LitIntn Integer  -- ^ integer  : ... | -1 | 0 | 1 | ...
  | LitFlon Double   -- ^ flonum   : -inf | ... | -0.1 | 0 | 0.1 | ... | +inf
  | LitRatn Rational -- ^ rational : 1/3 | 1/3 | 1/1 | ...
  deriving (Eq, Show)

data Term
  = TmLit   Lit            -- ^ literal
  | TmSym   Name           -- ^ symbol
  | TmVar   Int  Int       -- ^ λ 1     (no-name-variable-expression)
  | TmAbs   Name Type Term -- ^ λx:T.t  (lambda abstruction)
  | TmApp   Term Term      -- ^ f x     (apply abstruction to term)
  | TmTyAbs Name Type Term -- ^ λX<:T.t (type abstruction)
  | TmTyApp Term [Type]    -- ^ t [T]   (apply types to term)
  deriving (Eq, Show)

data Context
  = CtxEmpty               -- ^ ∅    (empty context)
  | CtxBindTerm Term Type  -- ^ λx.T (bind type to term)
  | CtxBindType Type Type  -- ^ λX.T (bind type to type)
  deriving (Eq, Show)

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
