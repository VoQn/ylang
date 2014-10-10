{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Ylang.Parser.Combinator where

import           Control.Applicative  hiding (many, (<|>))
import           Control.Monad
import           Text.Parsec
import           Text.Parsec.Text     (Parser)

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

infixl 3 !>

lexeme :: Parser a -> Parser a
lexeme p = p <* whiteSpace

whiteSpace :: Parser ()
whiteSpace = skipMany space

parseTop :: Parser a -> Parser a
parseTop p = p <* eof

parens :: Parser a -> Parser a
parens p = char '(' *> p <* char ')'

commaSep1 :: Parser a -> Parser [a]
commaSep1 p = (whiteSpace *> p) `sepBy1` char ','
