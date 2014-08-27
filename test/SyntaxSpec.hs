module SyntaxSpec (spec) where

import Data.Ratio ((%))

import Test.Hspec
import Test.QuickCheck

import Ylang.Syntax as Y

spec :: Spec
spec = do
  describe "ylang expression type can compare each other" $ do

    it "boolean (x) == boolean (x)" $
      property $ \ b -> Y.Boolean b == Y.Boolean b

  describe "ylang expression type can show own" $ do
    it "boolean (yes)" $
      show (Y.Boolean True) `shouldBe` "yes"

    it "boolean (no)" $
      show (Y.Boolean False) `shouldBe` "no"

    it "integer 0" $
      show (Y.Int 0) `shouldBe` "0"

    it "integer 10" $
      show (Y.Int 10) `shouldBe` "10"

    it "floating 0.5" $
      show (Y.Float 0.5) `shouldBe` "0.5"

    it "rational 1/10" $
      show (Y.Ratio (1 % 10)) `shouldBe` "1/10"

    it "string \"\"" $
      show (Y.String "") `shouldBe` "\"\""

    it "string \"message\"" $
      show (Y.String "message") `shouldBe` "\"message\""

    it "operator (&)" $
      show (Y.Atom "&") `shouldBe` "&"

    it "variable x" $
      show (Y.Atom "x") `shouldBe` "x"

    it "pair (x , y)" $
      show (Y.Pair (Y.Atom "x") (Y.Atom "y")) `shouldBe` "(x , y)"

    it "list []" $
      show (Y.Array []) `shouldBe` "[]"

    it "list [1 2 3]" $
      show (Y.Array [Y.Int 1, Y.Int 2,Y.Int 3]) `shouldBe` "[1 2 3]"

    it "lambda (-> x x)" $
      show (Y.Lambda (Y.Atom "x") [] (Y.Atom "x")) `shouldBe` "(-> x x)"

    it "lambda (-> (x y) y)" $
      show (Y.Lambda (Y.Atom "x") [Y.Atom "y"] (Y.Atom "y")) `shouldBe`
      "(-> (x y) y)"

    it "apply (f x y)" $
      show (Y.Call (Y.Atom "f") [Y.Atom "x", Y.Atom "y"]) `shouldBe`
      "(f x y)"

    it "arrow (-> A B)" $
      show (Y.Arrow　(Y.Atom "A") [] (Atom "B")) `shouldBe`
      "(-> A B)"

    it "arrow (-> A B C)" $
      show (Y.Arrow (Y.Atom "A") [Y.Atom "B"] (Atom "C")) `shouldBe`
      "(-> A B C)"

    it "arrow (-> A B C)" $
      show (Y.Arrow (Y.Atom "A") [Y.Atom "B"] (Atom "C")) `shouldBe`
      "(-> A B C)"

    it "define (= x 10)" $
      show (Y.Define (Y.Atom "x") [] (Y.Int 10)) `shouldBe`
      "(= x 10)"

    it "define (= (f x) x)" $
      show (Y.Define (Y.Atom "f") [Y.Atom "x"] (Atom "x")) `shouldBe`
      "(= (f x) x)"

    it "declare (: x Int)" $
      show (Y.Declare (Y.Atom "x") [] (Y.Atom "Int")) `shouldBe`
      "(: x Int)"

    it "declare (: (f A) B)" $
      show (Y.Declare (Y.Atom "f") [Y.Atom "A"] (Y.Atom "B")) `shouldBe`
      "(: (f A) B)"
