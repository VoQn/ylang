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

import Control.Applicative     ((<$>), (<$), (<*>), (*>), pure)
import Control.Monad.Identity  (Identity)
import Data.Char               (isSpace)
import Data.Ratio              ((%))
import Data.Text               (Text)
import Text.Parsec             (many1, satisfy, alphaNum, letter, oneOf)
import Text.Parsec.Text        (Parser)
import Ylang.Parser.Combinator ((</>), (<!))
import Ylang.Syntax.Literal    (Lit(..))

import qualified Data.Text               as T
import qualified Text.Parsec.Token       as TK

---------------------------------------------------------------------
-- Basic Lexer
---------------------------------------------------------------------

notSpace :: Parser Char
notSpace = satisfy $ not . isSpace

------------------------------------------------------------------
-- Language Definition of Ylang
------------------------------------------------------------------
type YlangDef st    = TK.GenLanguageDef Text st Identity

type TokenParser st = TK.GenTokenParser Text st Identity

ylangDef :: YlangDef ()
ylangDef = TK.LanguageDef {
    TK.commentStart    = ";;;"
  , TK.commentEnd      = ";;;"
  , TK.commentLine     = ";"
  , TK.nestedComments  = False
  , TK.identStart      = letter
  , TK.identLetter     = alphaNum
  , TK.opStart         = opChar
  , TK.opLetter        = opChar
  , TK.reservedNames   = reserves
  , TK.reservedOpNames = operators
  , TK.caseSensitive   = True
  }
  where
  opChar    = oneOf "~!@#$%^&*-+_=|\\:<>,./?"
  reserves  = []
  operators = ["=", "->", "=>", ":", "::", ",", ".", "-", "+", "*", "/"]

------------------------------------------------------------------
-- Lexer for Ylang
------------------------------------------------------------------

lexer :: TokenParser ()
lexer = TK.makeTokenParser ylangDef

parens :: Parser a -> Parser a
parens = TK.parens lexer

commaSep :: Parser a -> Parser [a]
commaSep = TK.commaSep lexer

semiSep :: Parser a -> Parser [a]
semiSep = TK.semiSep lexer

symbol :: String -> Parser String
symbol = TK.symbol lexer

reserved :: String -> Parser ()
reserved   = TK.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = TK.reservedOp lexer

------------------------------------------------------------------
-- Token
------------------------------------------------------------------

colon :: Parser ()
colon =  reservedOp ":"

dot :: Parser ()
dot = reservedOp "."

thinArrow :: Parser ()
thinArrow = reservedOp "->"

fatArrow :: Parser ()
fatArrow = reservedOp "=>"

hyphen :: Parser ()
hyphen = reservedOp "-"

slash :: Parser ()
slash = reservedOp "/"

identifier :: Parser String
identifier = TK.identifier lexer

whiteSpace :: Parser ()
whiteSpace = TK.whiteSpace lexer

lexeme :: Parser a -> Parser a
lexeme = TK.lexeme lexer

natural :: Parser Integer
natural = TK.natural lexer

sign :: (Num a) => Parser (a -> a)
sign = negate <$ hyphen </> pure id

integer :: Parser Integer
integer = TK.integer lexer

float :: Parser Double
float = ($) <$> sign <*> TK.float lexer

charLit :: Parser Char
charLit = TK.charLiteral lexer

strLit :: Parser String
strLit = TK.stringLiteral lexer

toText :: Parser String -> Parser Text
toText p = T.pack <$> p

rational :: Parser Rational
rational = ratio <$> sign <*> natural <*> (slash *> natural)
  where
  ratio f n d = f (n % d)

boolLit :: Parser Bool
boolLit
   =  True  <! "Yes"
  </> False <! "No"

keyword :: Parser Text
keyword = colon *> (T.pack <$> many1 notSpace)

------------------------------------------------------------------
-- Literal Token for Ylang
------------------------------------------------------------------

literal :: Parser Lit
literal
   =  LitHole <$  symbol "_"
  </> LitUnit <$  symbol "()"
  </> LitBool <$> boolLit
  </> LitRatn <$> rational
  </> LitFlon <$> float
  </> LitIntn <$> integer
  </> LitChr  <$> charLit
  </> LitStr  <$> toText strLit
  </> LitKey  <$> keyword
