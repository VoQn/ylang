module Ylang.Parser.Token where

import           Control.Applicative
import           Data.Char (isSpace)
import           Text.Parsec
import           Text.Parsec.Text (Parser)
import           Ylang.Parser.Combinator

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
