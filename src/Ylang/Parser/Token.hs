module Ylang.Parser.Token where

import Control.Applicative
import Data.Ratio
import Text.Parsec hiding (string)
import Text.Parsec.Text   (Parser)
import Ylang.Syntax.Literal
import Ylang.Parser.Combinator
import Ylang.Parser.Lexer

import qualified Data.Text as T

------------------------------------------------------------------
-- Literal Token for Ylang
------------------------------------------------------------------

hole :: Parser Lit
hole = LitHole <! "_"

unit :: Parser Lit
unit = LitUnit <! "()"

boolean :: Parser Lit
boolean = LitBool <$> (True <! "Yes" </> False <! "No")

integer :: Parser Lit
integer = LitIntn <$> intDigits
  where
  intDigits = sign <*> intNum

float :: Parser Lit
float = LitFlon <$> floating
  where
  floating = pack <$> sign <*> digits <*> char '.' <*> digits
  pack s i p f = s . read $ i ++ p : f

rational :: Parser Lit
rational = LitRatn <$> ratio
  where
  ratio = pack <$> sign <*> intNum <*> char '/' <*> intNum
  pack s n _ d = s $ n % d

charactor :: Parser Lit
charactor = '\'' !> LitChr <$> anyChar <* char '\''

string :: Parser Lit
string = '"' !> LitStr . T.pack <$> manyTill anyChar (try $ char '"')

keyword :: Parser Lit
keyword = ':' !> LitKey . T.pack <$> many1 notSpace
