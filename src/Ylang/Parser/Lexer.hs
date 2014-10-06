---------------------------------------------------------------------
-- |
-- Module      :  Ylang.Parser.Lexer
-- Description :  Token Lexer for Ylang
-- Copyright   :  (c) 2014 Kazuhiro Mizuhsima
-- License     :  Apache-2
--
-- Maintainer  :  Kazuhiro Mizushima <voqn.tyrantist@gmail.com>
-- Stability   :  unstable
-- Portability :  portable
---------------------------------------------------------------------
module Ylang.Parser.Lexer where

import           Control.Applicative  hiding (many, (<|>))
import           Data.Ratio
import           Text.Parsec
import           Text.Parsec.Text     (Parser)
import           Ylang.Syntax.Literal
import           Ylang.Parser.Combinator
import           Ylang.Parser.Token

import qualified Data.Text            as T

---------------------------------------------------------------------
-- Literal Token for Ylang
---------------------------------------------------------------------

tHole :: Parser Lit
tHole = LitHole <! "_"

tUnit :: Parser Lit
tUnit = LitUnit <! "()"

tBool :: Parser Lit
tBool = LitBool <$> (True <! "Yes" </> False <! "No")

tIntn :: Parser Lit
tIntn = LitIntn <$> tInteger
  where
  tInteger = tSig <*> tDig

tFlon :: Parser Lit
tFlon = LitFlon <$> tFloat
  where
  tFloat = pack <$> tSig <*> digits <*> char '.' <*> digits
  pack s i p f = s . read $ i ++ p : f

tRatn :: Parser Lit
tRatn = LitRatn <$> tRatio
  where
  tRatio = pack <$> tSig <*> tDig <*> char '/' <*> tDig
  pack s n _ d = s $ n % d

tChr :: Parser Lit
tChr = '\'' !> LitChr <$> anyChar <* char '\''

tStr :: Parser Lit
tStr = '"' !> LitStr . T.pack <$> manyTill anyChar (try $ char '"')

tKey :: Parser Lit
tKey = ':' !> LitKey . T.pack <$> many1 notSpace
