{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}
module Ylang.Display where

import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Builder as LB
import qualified Data.Text.Lazy.IO as LIO
import Data.Monoid

class Display a where
  textBuild :: a -> LB.Builder
  toString :: a -> String
  toString = L.unpack . LB.toLazyText . textBuild
  display :: a -> IO ()
  display = LIO.putStrLn . LB.toLazyText . textBuild

instance Display Char where
  textBuild = LB.singleton

instance Display String where
  textBuild = LB.fromString

instance Display Int where
  textBuild = LB.fromString . show

instance Display Integer where
  textBuild = LB.fromString . show

instance Display Double where
  textBuild = LB.fromString . show

instance Display LB.Builder where
  textBuild = id

mjoin :: Monoid t => t -> [t] -> t
mjoin _ [] = mempty
mjoin _ (x:[]) = x
mjoin s (x:xs) = x <> s <> mjoin s xs

spaceSep :: Display d => [d] -> LB.Builder
spaceSep = mjoin " " . map textBuild

close :: Char -> LB.Builder -> Char -> LB.Builder
close o b c = textBuild o <> b <> textBuild c

parens :: LB.Builder -> LB.Builder
parens b = close '(' b ')'

brackets :: LB.Builder -> LB.Builder
brackets b = close '[' b ']'

chrLit :: Char -> LB.Builder
chrLit c = close '\'' (textBuild c) '\''

strLit :: String -> LB.Builder
strLit s = "\"" <> (textBuild s) <> "\""
