{-# LANGUAGE OverloadedStrings #-}

module Ylang.Display where

-- import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Builder as LB
import Data.Monoid

close :: Char -> LB.Builder -> Char -> LB.Builder
close o b c = LB.singleton o <> b <> LB.singleton c

parens :: LB.Builder -> LB.Builder
parens b = close '(' b ')'

brackets :: LB.Builder -> LB.Builder
brackets b = close '[' b ']'

chrLit :: Char -> LB.Builder
chrLit c = close '\'' (LB.singleton c) '\''

strLit :: String -> LB.Builder
strLit s = "\"" <> (LB.fromString s) <> "\""
