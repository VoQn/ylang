{-# LANGUAGE OverloadedStrings #-}

module Ylang.Syntax where

import qualified Data.Text as T hiding (singleton)
import qualified Data.Text.Lazy.Builder as T

import Data.Monoid

import Data.Ratio (numerator, denominator)
import Data.List (intercalate)

type Name = String

data Expr
  -- atomic
  = Atom    Name
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
  | Void
  | Factor [Expr]

  -- anonymous function
  | Func {
      arg1 :: Expr,
      args :: [Expr],
      prem :: [Expr],
      retn :: Expr
    }

  | Arrow Expr [Expr] Expr

  -- declaration
  | Declare {
      name :: Name,
      prem :: [Expr],
      argT :: [Expr],
      retT :: Expr
    }
  -- definition
  | Define {
      name :: Name,
      retn :: Expr
    }
  -- redundant
  | Call    Expr [Expr]

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
mjoin s es = mjoin' mempty s es

mjoin' :: Monoid t => [t] -> t -> [t] -> t
mjoin' rs s es = case es of
  []      -> mconcat rs
  (e:es') ->
    let
      rs' = case rs of
        [] -> [e]
        _  -> rs ++ [s, e]
    in mjoin' rs' s es'


toText :: Expr -> T.Builder
toText expr = case expr of
  -- atomic expression
  Atom    s -> toTB s
  Keyword k -> (T.singleton ':') <> (toTB k)
  Int     n -> toTB $ show n
  Float   n -> toTB $ show n
  Ratio   v ->
    let n = numerator v
        d = denominator v
        c = toTB . show
    in (c n) <> "/" <> (c d)
  Char    c -> mconcat $ map T.singleton ['\'', c, '\'']
  String  s -> "\"" <> toTB s <> "\""
  Boolean b | b -> "yes" | otherwise -> "no"

  -- collection expression
  Pair e1 e2 -> "(, " <> (spSep [e1,e2]) <> ")"
  Array es   -> "[" <> (spSep es) <> "]"

  -- factor
  Void -> "()"
  Factor as -> "(" <> (spSep as) <> ")"

  Func i as es r ->
    let
      as' = case as of
        [] -> toText i
        _ -> "(" <> (spSep (i:as)) <> ")"
      es' = case es of
        [] -> toText r
        _  -> "(" <> (spSep (es ++ [r])) <> ")"
    in "(" <> (mjoin " " ["\\", as', es']) <> ")"

  Call e1 e2 -> "(" <> (spSep (e1:e2)) <> ")"

  Arrow i as r
    -> "(-> " <> (spSep (i : as ++ [r])) <> ")"

  Define n v ->
    let
      exps = case v of
        Func i as es r ->
          let
            args' = spSep $ i : as
            func = "(" <> (toTB n) <> " " <> args' <> ")"
            body = case es of
              [] -> toText r
              _  -> "(" <> (spSep (es ++ [r])) <> ")"
          in [func, body]
        _ -> [toTB n, toText v]
    in "(= " <> (mjoin " " exps) <> ")"

  Declare n pm as r ->
    let rets = case as of
          [] -> toText r
          _  -> "(-> " <> (spSep $ as ++ [r]) <> ")"
        prems = map toText pm
    in "(: " <> (mjoin " " ((toTB n) : prems ++ [rets])) <> ")"
  where
  spSep = mjoin " " . map toText
