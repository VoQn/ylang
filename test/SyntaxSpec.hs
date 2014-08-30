module SyntaxSpec (spec) where

import Data.Ratio ((%))

import Test.Hspec
import Test.QuickCheck

import Ylang.Syntax

spec :: Spec
spec = do
  describe "ylang expression type can compare each other" $ do

    it "boolean (x) == boolean (x)" $
      property $ \ b -> Boolean b == Boolean b

    it "integer (x) == integer (x)" $
      property $ \ x -> Int x == Int x

    it "floating (x) == floating (x)" $
      property $ \ x -> Float x == Float x

    it "rational (x) == rational (x)" $
      property $ \ x -> Ratio x == Ratio x

  describe "ylang expression type can show own" $ do
    it "boolean (yes)" $
      show (Boolean True) `shouldBe` "yes"

    it "boolean (no)" $
      show (Boolean False) `shouldBe` "no"

    it "integer 0" $
      show (Int 0) `shouldBe` "0"

    it "integer 10" $
      show (Int 10) `shouldBe` "10"

    it "floating 0.5" $
      show (Float 0.5) `shouldBe` "0.5"

    it "rational 1/10" $
      show (Ratio (1 % 10)) `shouldBe` "1/10"

    it "string \"\"" $
      show (String "") `shouldBe` "\"\""

    it "string \"message\"" $
      show (String "message") `shouldBe` "\"message\""

    it "operator (&)" $
      show (Atom "&") `shouldBe` "&"

    it "variable x" $
      show (Atom "x") `shouldBe` "x"

    it "pair (, x y)" $
      show (Pair (Atom "x") (Atom "y")) `shouldBe` "(, x y)"

    it "list []" $
      show (Array []) `shouldBe` "[]"

    it "list [1 2 3]" $
      show (Array [Int 1,Int 2,Int 3]) `shouldBe` "[1 2 3]"

    it "void ()" $
      show Void `shouldBe` "()"

    it "Func (\\ x x)" $
      show (Func (Atom "x") [] [] (Atom "x")) `shouldBe` "(\\ x x)"

    it "Func (\\ (x y) y)" $
      show (Func (Atom "x") [Atom "y"] [] (Atom "y")) `shouldBe`
      "(\\ (x y) y)"

    it "apply (f x y)" $
      show (Factor [Atom "f",Atom "x",Atom "y"]) `shouldBe`
      "(f x y)"

    it "arrow (-> A B)" $
      show (Factor [Atom "->",Atom "A",Atom "B"]) `shouldBe`
      "(-> A B)"

    it "arrow (-> A B C)" $
      show (Factor [Atom "->",Atom "A",Atom "B",Atom "C"]) `shouldBe`
      "(-> A B C)"

    it "arrow (-> (A B) C)" $
      show (Factor [Atom "->",Factor [Atom "A",Atom "B"],Atom "C"]) `shouldBe`
      "(-> (A B) C)"

    it "define (= x 10)" $
      show (Define "x" $ Int 10) `shouldBe`
      "(= x 10)"

    it "define (= (f x) x)" $
      show (Define "f" $ Func (Atom "x") [] [] (Atom "x")) `shouldBe`
      "(= (f x) x)"

    it "declare (: x Int)" $
      show (Declare "x" [] [] $ Atom "Int") `shouldBe`
      "(: x Int)"

    it "declare (: f (-> A B))" $
      show (Declare "f" [] [Atom "A"] $ Atom "B") `shouldBe`
      "(: f (-> A B))"

    it "declare (: f (-> A B C))" $
      show (Declare "f" [] [Atom "A",Atom "B"] $ Atom "C") `shouldBe`
      "(: f (-> A B C))"

    it "declare (: f (: t A) (-> t t)))" $ do
      let premises = [Declare "t" [] [] (Atom "A")]
      let expr = Declare "f" premises [Atom "t"] $ Atom "t"
      show expr `shouldBe` "(: f (: t A) (-> t t))"

  describe "currying function" $ do

    it "currying (\\ (x y) y) => (\\ x (\\ y y))" $ do
      let f = Func (Atom "x") [Atom "y"] [] (Atom "y")
      let g = Func (Atom "x") [] [] (Func (Atom "y") [] [] (Atom "y"))
      currying f `shouldBe` g
