{-# LANGUAGE OverloadedStrings #-}

module Ylang.Syntax
 (
  Name,
  Expr(..),
  showRatio,
  currying,
  toText
 ) where

import qualified Data.Text as T hiding (singleton)
import qualified Data.Text.Lazy.Builder as T

import Data.Monoid

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

toTB :: String -> T.Builder
toTB = T.fromText . T.pack

mjoin :: Monoid t => t -> [t] -> t
mjoin = mjoin' mempty

mjoin' :: Monoid t => [t] -> t -> [t] -> t
mjoin' rs _ []     = mconcat rs
mjoin' [] s (e:es) = mjoin' [e] s es
mjoin' rs s (e:es) = mjoin' (rs ++ [s, e]) s es

spSep :: [Expr] -> T.Builder
spSep = mjoin " " . map toText

toText :: Expr -> T.Builder
-- atomic expression
toText (Void) = "()"

toText (Boolean True)  = "yes"
toText (Boolean False) = "no"

toText (Atom s)    = toTB s
toText (Keyword k) = ":" <> (toTB k)

toText (Int n)     = toTB $ show n
toText (Float n)   = toTB $ show n
toText (Ratio v) =
  let n = numerator v
      d = denominator v
      c = toTB . show
  in (c n) <> "/" <> (c d)

toText (Char c)   = chrLit c
toText (String s) = strLit s

-- collection expression
toText (Pair e1 e2) = parens $ ", " <> spSep [e1,e2]
toText (Array es)   = brackets $ spSep es

-- factor
toText (Call e1 e2)   = parens $ spSep (e1 : e2)
toText (Arrow i as r) = parens $ "-> " <> spSep (i : as ++ [r])

toText (Func i as es r) = parens $ mjoin " " ["\\", as', es']
  where
  as' = case as of
    [] -> toText i
    _  -> parens $ spSep (i : as)
  es' = case es of
    [] -> toText r
    _  -> spSep (es ++ [r])

toText (Define n v) = parens $ "= " <> mjoin " " exps
  where
  exps = case v of
    Func i as es r -> [func, body]
      where
        args' = spSep $ i : as
        func = parens $ (toTB n) <> " " <> args'
        body = case es of
          []  -> toText r
          _   -> spSep (es ++ [r])
    _ -> [toTB n, toText v]

toText (Declare n pm as r) = parens $ ": " <> mjoin " " (n' : pr ++ [rt])
  where
  n' = toTB n
  rt = case as of
    [] -> toText r
    _  -> parens $ "-> " <> spSep (as ++ [r])
  pr = map toText pm
