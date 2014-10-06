---------------------------------------------------------------------
-- |
-- Module      :  Ylang.Parser.Lexer
-- Description :  Token Lexer for Ylang
-- Copyright   :  (c) 2014 Kazuhiro Mizuhsima
-- License     :  Apache-2
--
-- Maintainer  :  Kazuhiro Mizushima <voqn.tyrantist@gmail.com>
-- Stability   :  unstable
-- Portability :  non-portable (Using -TypeSynonymInstances, -XFlexibleInstances)
---------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Ylang.Parser.Lexer where

import           Control.Applicative  hiding (many, (<|>))
import           Control.Monad
import           Data.Char
import           Data.Ratio
import           Text.Parsec
import           Text.Parsec.Text     (Parser)
import           Ylang.Syntax.Literal

import qualified Data.Text            as T
---------------------------------------------------------------------
-- Tokenization
---------------------------------------------------------------------

class Token a where
  tokenize :: a -> Parser ()

instance Token Char where
  tokenize = void . char

instance Token String where
  tokenize = void . string

---------------------------------------------------------------------
-- Combinator
---------------------------------------------------------------------

(</>) :: Parser a -> Parser a -> Parser a
p1 </> p2 = try p1 <|> p2

infixr 1 </>

(<!) :: (Token t) => a -> t -> Parser a
r <! t = r <$ tokenize t

infixr 3 <!

(!>) :: (Token t) => t -> Parser a -> Parser a
t !> r = tokenize t *> r

infix 3 !>

---------------------------------------------------------------------
-- Basic Token Parsers
---------------------------------------------------------------------

notSpace :: Parser Char
notSpace = satisfy $ not . isSpace

digits :: Parser String
digits = many1 digit

tSig :: (Num a) => Parser (a -> a)
tSig = '-' !> pure negate </> pure id

tDig :: (Read a, Num a) => Parser a
tDig = read <$> digits

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
