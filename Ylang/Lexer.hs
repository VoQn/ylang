module Ylang.Lexer where

-- import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
-- import Text.Parsec.Prim (many)

import qualified Text.Parsec.Token as Token

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser style
  where
    style = emptyDef {
        Token.commentLine     = commentLine
      , Token.reservedOpNames = operators
      , Token.reservedNames   = keywords
    }
    commentLine = ";"
    operators   = ["+", "-", "*", "/", "->", ",", ".", ":"]
    keywords    = ["dec", "def", "let", "var"]

integer = Token.integer lexer
float   = Token.float lexer
strings = Token.stringLiteral lexer

parens     = Token.parens lexer
brackets   = Token.brackets lexer

reserved   = Token.reserved lexer
reservedOp = Token.reservedOp lexer
