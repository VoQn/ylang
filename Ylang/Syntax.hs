{-# LANGUAGE OverloadedStrings #-}

module Ylang.Syntax
 (
  Name,
  Expr(..),
  currying
 ) where

import Data.Monoid ((<>))
import Data.Ratio (numerator, denominator)

import Ylang.Display

type Name = String

data Expr
  -- atomic
  = Void
  | Atom    Name
  | Keyword Name
  | Int     Integer
  | Float   Double
  | Ratio   Rational
  | Char    Char
  | String  String
  | Boolean Bool

  -- collection
  | Pair  Expr   Expr
  | Array [Expr]

  -- factor
  | Call Expr [Expr]

  -- anonymous function
  | Func Expr [Expr] [Expr] Expr

  -- function type
  | Arrow Expr [Expr] Expr

  -- declaration
  | Declare Name [Expr] [Expr] Expr

  -- definition
  | Define Name Expr
  deriving (Eq, Ord, Show)

currying :: Expr -> Expr
currying f = case f of
  -- currring avail with function
  Func _ [] _ _ -> f
  Func i (a:as) es r ->
    currying $ Func i as [] $ Func a [] es r

  _ -> f

instance Display Expr where
  textBuild e = case e of
    -- atomic expression
    Void   -> "()"
    Atom s -> textBuild s

    Boolean True  -> "yes"
    Boolean False -> "no"

    Char   c -> chrLit c
    String s -> strLit s

    Keyword k -> ":" <> textBuild k

    Int   n -> textBuild n
    Float n -> textBuild n
    Ratio v -> numer v <> "/" <> denom v
      where
        numer = textBuild . numerator
        denom = textBuild . denominator

    -- collection expression
    Pair e1 e2 -> parens $ ", " <> spaceSep [e1, e2]
    Array es   -> brackets $ spaceSep es

    -- factor
    Call e1 e2   -> parens $ spaceSep $ e1 : e2
    Arrow i as r -> parens $ "-> " <> spaceSep ((i : as) ++ [r])

    Func i as es r
      -> parens $ spaceSep ["\\", form i as, body r es]
      where
      form x [] = textBuild x
      form x xs = parens $ spaceSep (x:xs)
      body x [] = textBuild x
      body x xs = spaceSep $ xs ++ [x]

    Define n (Func i as es r)
      -> parens $ spaceSep ["=", func, body es r]
      where
      func = parens $ spaceSep form
      form = textBuild n : map textBuild (i : as)
      body [] y = textBuild y
      body xs y = spaceSep (xs ++ [y])

    Define n v
      -> parens $ spaceSep ["=", textBuild n, textBuild v]

    Declare n pm as r
      -> parens $ ": " <> spaceSep ((textBuild n) : pr ++ [ret as r])
      where
      ret [] y = textBuild y
      ret xs y = parens $ "-> " <> spaceSep (xs ++ [y])
      pr = map textBuild pm
