module Ylang.Parser.Atomic
  ( variable
  , operator
  , identifier
  , symbol
  , atom
  , number
  , ratio
  , int
  , float
  , bool
  , keyword
  , chr
  , str
  ) where

import Data.Ratio ((%))

import Text.Parsec
  ( parse, try, many, many1, oneOf
  , space, letter, alphaNum, char, string
  , (<|>), (<?>)
  )
import Text.Parsec.String (Parser)

import Control.Applicative ((<$>), (<*>), (<*), (*>))

import Ylang.Lexer
  (reservedOp, integer, floating, strings, charLit)

import Ylang.Syntax
  (Expr(Atom, Ratio, Int, Float, Boolean, Keyword, Char, String))

-- |
-- Parse Variables
-- >>> parse variable "<stdin>" "x"
-- Right (Atom "x")
-- >>> parse variable "<stdin>" "empty?"
-- Right (Atom "empty?")
-- >>> parse variable "<stdin>" "add#"
-- Right (Atom "add#")
-- >>> parse variable "<stdin>" "x->string"
-- Right (Atom "x->string")
variable :: Parser Expr
variable = Atom <$> identifier

-- |
-- Parse Operator
-- >>> parse operator "<stdin>" "+"
-- Right (Atom "+")
-- >>> parse operator "<stdin>" "??"
-- Right (Atom "??")
operator :: Parser Expr
operator = Atom <$> many1 symbol <* many space

-- |
-- Parse Symbol Identifier
-- >>> parse identifier "<stdin>" "x"
-- Right "x"
-- >>> parse identifier "<stdin>" "x->string"
-- Right "x->string"
identifier :: Parser String
identifier = (:) <$> istart <*> ichars <* many space
  where
  istart = letter
  ichars = many $ alphaNum <|> symbol

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

atom :: Parser Expr
atom
   =  try number
  <|> try bool
  <|> try chr
  <|> try str
  <|> try operator
  <|> try keyword
  <|> variable
  <?> "Atomic Value"

-- |
-- Parse Keyword literal [:keyword, :test, :include-symbol ... ]
-- >>> parse keyword "<stdin>" ":dont-look-back"
-- Right (Keyword "dont-look-back")
keyword :: Parser Expr
keyword = Keyword <$> (istart *> ichars <* many space)
  where
  istart = char ':'
  ichars = many $ alphaNum <|> symbol

number :: Parser Expr
number
   =  try float
  <|> try ratio
  <|> int
  <?> "Number Literal"

-- |
-- Parse Rational number literal [-1/2, 0/1, 1/2, 3/5, ...]
-- >>> parse ratio "<stdin>" "1/2"
-- Right (Ratio (1 % 2))
-- >>> parse ratio "<stdin>" "4/2"
-- Right (Ratio (2 % 1))
ratio :: Parser Expr
ratio = do
  n <- integer
  reservedOp "/"
  d <- integer
  return $ Ratio $ n % d

-- |
-- Parse integer number [...-2 -1, 0, 1 2 ...]
-- >>> parse int "<stdin>" "0"
-- Right (Int 0)
-- >>> parse int "<stdin>" "-2"
-- Right (Int (-2))
-- >>> parse int "<stdin>" "0xff"
-- Right (Int 255)
int :: Parser Expr
int = Int <$> integer

-- |
-- Parse flaoting point number [... -1.0 -0.5 0.0 0.5 1.0 ...]
-- >>> parse float "<stdin>" "0.0"
-- Right (Float 0.0)
-- >>> parse float "<stdin>" "0.5"
-- Right (Float 0.5)
-- >>> parse float "<stdin>" "-0.5"
-- Right (Float (-0.5))
float :: Parser Expr
float = wrap <$> signed <*> floating
  where
  wrap s v = Float $ s v
  signed   = try minus <|> return id
  minus    = char '-' >> return ((-) 0)

-- |
-- Parse boolean identifier [Yes/No]
-- >>> parse bool "<stdin>" "yes"
-- Right (Boolean True)
-- >>> parse bool "<stdin>" "no"
-- Right (Boolean False)
bool :: Parser Expr
bool = Boolean <$> ("yes" ?> True <|> "no" ?> False)
  where
  k ?> r = try (string k) >> return r

-- |
-- Parse Charactor literal
-- >>> parse chr "<stdin>" "'c'"
-- Right (Char 'c')
chr :: Parser Expr
chr = Char <$> charLit

-- |
-- Parse String literal
-- >>> parse str "<stdin>" "\"\""
-- Right (String "")
-- >>> parse str "<stdin>" "\"Hello!\""
-- Right (String "Hello!")
str :: Parser Expr
str = String <$> strings
