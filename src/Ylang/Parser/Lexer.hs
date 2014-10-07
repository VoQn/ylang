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

import Control.Applicative  hiding (many, (<|>))
import Data.Char
import Text.Parsec
import Text.Parsec.Text     (Parser)
import Ylang.Parser.Combinator

---------------------------------------------------------------------
-- Basic Lexer
---------------------------------------------------------------------

notSpace :: Parser Char
notSpace = satisfy $ not . isSpace

digits :: Parser String
digits = many1 digit

sign :: (Num a) => Parser (a -> a)
sign = '-' !> pure negate </> pure id

intNum :: (Read a, Num a) => Parser a
intNum = read <$> digits
