{-# LANGUAGE OverloadedStrings #-}

module Ylang.Syntax
 (
  Name,
  Expr(..),
  currying
 ) where

import Data.Text.Lazy.Builder (Builder)

import Data.Monoid ((<>))

import Data.Ratio (numerator, denominator)
import Data.List (intercalate)

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

showRatio :: Rational -> String
showRatio x
  = intercalate "/" $ map (show . ($ x)) [numerator, denominator]

currying :: Expr -> Expr
currying f = case f of
  -- currring avail with function
  Func _ [] _ _ -> f
  Func i (a:as) es r ->
    currying $ Func i as [] $ Func a [] es r

  _ -> f

instance Display Expr where
  textBuild = toText

toText :: Expr -> Builder
-- atomic expression
toText (Void) = "()"

toText (Boolean True)  = "yes"
toText (Boolean False) = "no"

toText (Atom s)    = textBuild s
toText (Keyword k) = ":" <> textBuild k

toText (Int n)     = textBuild n
toText (Float n)   = textBuild n
toText (Ratio v) =
  let n = textBuild $ numerator v
      d = textBuild $ denominator v
  in n <> "/" <> d

toText (Char c)   = chrLit c
toText (String s) = strLit s

-- collection expression
toText (Pair e1 e2) = parens $ ", " <> spaceSep [e1, e2]
toText (Array es)   = brackets $ spaceSep es

-- factor
toText (Call e1 e2)   = parens $ spaceSep $ e1 : e2
toText (Arrow i as r) = parens $ "-> " <> spaceSep ((i : as) ++ [r])

toText (Func i as es r) = parens $ spaceSep ["\\", as', es']
  where
  as' = case as of
    [] -> textBuild i
    _  -> parens $ spaceSep (i : as)
  es' = case es of
    [] -> textBuild r
    _  -> spaceSep (es ++ [r])

toText (Define n v) = parens $ "= " <> spaceSep exps
  where
  exps = case v of
    Func i as es r -> [func, body]
      where
        arg' = spaceSep $ i : as
        func = parens $ textBuild n <> " " <> arg'
        body = case es of
          []  -> textBuild r
          _   -> spaceSep (es ++ [r])
    _ -> [textBuild n, textBuild v]

toText (Declare n pm as r) = parens $ ": " <> spaceSep (n' : pr ++ [rt])
  where
  n' = textBuild n
  rt = case as of
    [] -> textBuild r
    _  -> parens $ "-> " <> spaceSep (as ++ [r])
  pr = map textBuild pm
