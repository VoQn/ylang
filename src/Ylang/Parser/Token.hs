module Ylang.Parser.Token where

import Control.Applicative
import Text.Parsec.Text   (Parser)
import Ylang.Syntax.Literal
import Ylang.Parser.Combinator
import Ylang.Parser.Lexer

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
