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

sign :: (Num a) => Parser (a -> a)
sign = '-' !> pure negate </> pure id

intNum :: (Read a, Num a) => Parser a
intNum = read <$> digits
