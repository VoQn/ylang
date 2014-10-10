module Ylang.Parser.Token where

import Control.Applicative
import Control.Monad.Identity
import Data.Ratio
import Data.Text  (Text)
import Text.Parsec hiding (string)

import Text.Parsec.Text   (Parser)
import Ylang.Syntax.Literal
import Ylang.Parser.Combinator
import Ylang.Parser.Lexer

import qualified Data.Text as T
import qualified Text.Parsec.Token as TK

type YlangDef st    = TK.GenLanguageDef Text st Identity
type TokenParser st = TK.GenTokenParser Text st Identity

------------------------------------------------------------------
-- Language Definition of Ylang
------------------------------------------------------------------

ylangDef :: YlangDef ()
ylangDef = TK.LanguageDef {
    TK.commentStart    = ";;;"
  , TK.commentEnd      = ";;;"
  , TK.commentLine     = ";"
  , TK.nestedComments  = False
  , TK.identStart      = undefined
  , TK.identLetter     = undefined
  , TK.opStart         = undefined
  , TK.opLetter        = undefined
  , TK.reservedNames   = []
  , TK.reservedOpNames = []
  , TK.caseSensitive   = True
  }

------------------------------------------------------------------
-- Lexer for Ylang
------------------------------------------------------------------

lexer :: TokenParser ()
lexer = TK.makeTokenParser ylangDef

symbol :: String -> Parser String
symbol = TK.symbol lexer

natural :: Parser Integer
natural = TK.natural lexer

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
rational = ratio <$> sign <*> natural <*> (char '/' *> natural)
  where
  ratio f n d = f (n % d)

boolLit :: Parser Bool
boolLit
   =  True  <$ symbol "Yes"
  </> False <$ symbol "No"

keyword :: Parser Text
keyword = T.pack <$> (char ':' *> many1 notSpace)

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
