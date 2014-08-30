module Ylang.Lexer where

import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)

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
    operators   = ["+", "-", "*", "/", "=", "\\", "->", ",", ".", ":"]
    keywords    = ["dec", "def", "let", "var"]

integer :: Parser Integer
integer = Token.integer lexer

floating :: Parser Double
floating = Token.float lexer

strings :: Parser String
strings = Token.stringLiteral lexer

charLit :: Parser Char
charLit = Token.charLiteral lexer

parens :: Parser a -> Parser a
parens = Token.parens lexer

brackets :: Parser a -> Parser a
brackets = Token.brackets lexer

reserved :: String -> Parser ()
reserved = Token.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer
