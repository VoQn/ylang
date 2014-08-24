module Main where

import Test.DocTest

main :: IO ()
main = doctest ["Parser.hs", "Eval.hs"]
