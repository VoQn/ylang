module Main where

import Test.DocTest

main :: IO ()
main = doctest ["Ylang/Parser.hs", "Ylang/Eval.hs"]
