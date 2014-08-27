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
      show (Y.Var "&") `shouldBe` "&"

    it "variable x" $
      show (Y.Var "x") `shouldBe` "x"

    it "list []" $
      show (Y.List []) `shouldBe` "[]"

    it "list [1 2 3]" $
      show (Y.List [Y.Int 1, Y.Int 2,Y.Int 3]) `shouldBe` "[1 2 3]"

    it "lambda (-> x x)" $
      show (Y.Lambda (Y.Var "x") [] (Y.Var "x")) `shouldBe` "(-> x x)"

    it "lambda (-> (x y) y)" $
      show (Y.Lambda (Y.Var "x") [Y.Var "y"] (Y.Var "y")) `shouldBe`
      "(-> (x y) y)"

    it "apply (f x y)" $
      show (Y.Call (Y.Var "f") [Y.Var "x", Y.Var "y"]) `shouldBe`
      "(f x y)"

    it "arrow (-> A B)" $
      show (Y.Arrow　(Y.Var "A") [] (Var "B")) `shouldBe`
      "(-> A B)"

    it "arrow (-> A B C)" $
      show (Y.Arrow (Y.Var "A") [Y.Var "B"] (Var "C")) `shouldBe`
      "(-> A B C)"

    it "arrow (-> A B C)" $
      show (Y.Arrow (Y.Var "A") [Y.Var "B"] (Var "C")) `shouldBe`
      "(-> A B C)"

    it "define (= x 10)" $
      show (Y.Define (Y.Var "x") [] (Y.Int 10)) `shouldBe`
      "(= x 10)"

    it "define (= (f x) x)" $
      show (Y.Define (Y.Var "f") [Y.Var "x"] (Var "x")) `shouldBe`
      "(= (f x) x)"

    it "declare (: x Int)" $
      show (Y.Declare (Y.Var "x") [] (Y.Var "Int")) `shouldBe`
      "(: x Int)"

    it "declare (: (f A) B)" $
      show (Y.Declare (Y.Var "f") [Y.Var "A"] (Y.Var "B")) `shouldBe`
      "(: (f A) B)"
