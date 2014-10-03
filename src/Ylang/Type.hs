module Ylang.Type
  ( module Ylang.Type.Kind
  , Type(..)
  ) where

import Ylang.Info
import Ylang.Type.Kind

data Type
  = TyTop                  -- ^ Top type
  | TyBottom               -- ^ Bottom type
  | TyUnit                 -- ^ Unit type ()
  | TyVar   Int  Int       -- ^ type-variable
  | TyArrow Type Type      -- ^ T -> T
  | TyAll   Name Type      -- ^ ∀X<:T.T
  | TyAbs   Name Kind Type -- ^ λX::K.T
  | TyApp   Type Type      -- ^ T T
  deriving (Eq, Show)
