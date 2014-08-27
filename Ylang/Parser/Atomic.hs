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
  , str
  ) where

import Data.Ratio ((%))

import Text.Parsec
  ( parse, try, many, many1, oneOf
  , space, letter, alphaNum, char, string
  , (<|>), (<?>)
  )
import Text.Parsec.String (Parser)

import Control.Applicative ((<$>), (<*>), (<*))

import Ylang.Lexer
  (reservedOp, integer, floating, strings)
  
import Ylang.Syntax
  (Expr(Atom, Ratio, Int, Float, Boolean, String))

-- |
-- Parse Variables
-- >>> parse variable "<stdin>" "x"
-- Right x
-- >>> parse variable "<stdin>" "empty?"
-- Right empty?
-- >>> parse variable "<stdin>" "add#"
-- Right add#
-- >>> parse variable "<stdin>" "x->string"
-- Right x->string
variable :: Parser Expr
variable = Atom <$> identifier

-- |
-- Parse Operator
-- >>> parse operator "<stdin>" "+"
-- Right +
-- >>> parse operator "<stdin>" "??"
-- Right ??
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
  <|> try str
  <|> try operator
  <|> variable
  <?> "Atomic Value"

number :: Parser Expr
number
   =  try float
  <|> try ratio
  <|> int
  <?> "Number Literal"

-- |
-- Parse Rational number literal [-1/2, 0/1, 1/2, 3/5, ...]
-- >>> parse ratio "<stdin>" "1/2"
-- Right 1/2
-- >>> parse ratio "<stdin>" "4/2"
-- Right 2/1
ratio :: Parser Expr
ratio = do
  n <- integer
  reservedOp "/"
  d <- integer
  return $ Ratio $ n % d

-- |
-- Parse integer number [...-2 -1, 0, 1 2 ...]
-- >>> parse int "<stdin>" "0"
-- Right 0
-- >>> parse int "<stdin>" "-2"
-- Right -2
-- >>> parse int "<stdin>" "0xff"
-- Right 255
int :: Parser Expr
int = Int <$> integer

-- |
-- Parse flaoting point number [... -1.0 -0.5 0.0 0.5 1.0 ...]
-- >>> parse float "<stdin>" "0.0"
-- Right 0.0
-- >>> parse float "<stdin>" "0.5"
-- Right 0.5
-- >>> parse float "<stdin>" "-0.5"
-- Right -0.5
float :: Parser Expr
float = wrap <$> signed <*> floating
  where
  wrap s v = Float $ s v
  signed   = try minus <|> return id
  minus    = char '-' >> return ((-) 0)

-- |
-- Parse boolean identifier [Yes/No]
-- >>> parse bool "<stdin>" "yes"
-- Right yes
-- >>> parse bool "<stdin>" "no"
-- Right no
bool :: Parser Expr
bool = Boolean <$> ("yes" ?> True <|> "no" ?> False)
  where
  k ?> r = try (string k) >> return r

-- |
-- Parse String literal
-- >>> parse str "<stdin>" "\"\""
-- Right ""
-- >>> parse str "<stdin>" "\"Hello!\""
-- Right "Hello!"
str :: Parser Expr
str = String <$> strings
