{-# LANGUAGE OverloadedStrings #-}

module SyntaxSpec (spec) where

import Data.Ratio ((%))

import Test.Hspec
import Test.QuickCheck

import Ylang.Syntax
import Ylang.Display

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

    it "charactor (c) == charactor (c)" $
      property $ \ c -> Char c == Char c

  describe "ylang expression type can show own" $ do
    it "boolean (yes)" $
      toString (Boolean True) `shouldBe` "yes"

    it "boolean (no)" $
      toString (Boolean False) `shouldBe` "no"

    it "integer 0" $
      toString (Int 0) `shouldBe` "0"

    it "integer 10" $
      toString (Int 10) `shouldBe` "10"

    it "floating 0.5" $
      toString (Float 0.5) `shouldBe` "0.5"

    it "rational 1/10" $
      toString (Ratio (1 % 10)) `shouldBe` "1/10"

    it "charactor 'c'" $
      toString (Char 'c') `shouldBe` "'c'"

    it "string \"\"" $
      toString (String "") `shouldBe` "\"\""

    it "string \"message\"" $
      toString (String "message") `shouldBe` "\"message\""

    it "keyword :key" $
      toString (Keyword "key") `shouldBe` ":key"

    it "operator (&)" $
      toString (Atom "&") `shouldBe` "&"

    it "variable x" $
      toString (Atom "x") `shouldBe` "x"

    it "pair (, x y)" $
      toString (Pair (Atom "x") (Atom "y")) `shouldBe` "(, x y)"

    it "list []" $
      toString (Array []) `shouldBe` "[]"

    it "list [1 2 3]" $
      toString (Array [Int 1,Int 2,Int 3]) `shouldBe` "[1 2 3]"

    it "void ()" $
      toString Void `shouldBe` "()"

    it "func (\\ x x)" $
      toString (Func (Atom "x") [] [] (Atom "x")) `shouldBe`
      "(\\ x x)"

    it "func (\\ (x y) y)" $
      toString (Func (Atom "x") [Atom "y"] [] (Atom "y")) `shouldBe`
      "(\\ (x y) y)"

    it "func (\\ (x y) (= z (+ x y)) z)" $ do
      let g   = Define "z" $ Call (Atom "+") [Atom "x",Atom "y"]
      let f r = Func (Atom "x") [Atom "y"] [r] $ Atom "z"
      toString (f g) `shouldBe` "(\\ (x y) (= z (+ x y)) z)"

    it "apply (f x y)" $
      toString (Call (Atom "f") [Atom "x",Atom "y"]) `shouldBe`
      "(f x y)"

    it "arrow (-> A B)" $
      toString (Arrow (Atom "A") [] (Atom "B")) `shouldBe`
      "(-> A B)"

    it "arrow (-> A B C)" $
      toString (Arrow (Atom "A") [Atom "B"] (Atom "C")) `shouldBe`
      "(-> A B C)"

    it "arrow (-> (A B) C)" $
      toString (Arrow (Call (Atom "A") [Atom "B"]) [] (Atom "C")) `shouldBe`
      "(-> (A B) C)"

    it "define (= x 10)" $
      toString (Define "x" $ Int 10) `shouldBe`
      "(= x 10)"

    it "define (= (f x) x)" $
      toString (Define "f" $ Func (Atom "x") [] [] (Atom "x")) `shouldBe`
      "(= (f x) x)"

    it "define (= (f x) (\\ z (+ z 1)) (z x))" $ do
      let h = Func (Atom "z") [] [] $ Call (Atom "+") [Atom "z", Int 1]
      let g f = Func (Atom "x") [] [f] $ Call (Atom "z") [Atom "x"]
      toString (Define "f" $ g h) `shouldBe` "(= (f x) (\\ z (+ z 1)) (z x))"

    it "declare (: x Int)" $
      toString (Declare "x" [] [] $ Atom "Int") `shouldBe`
      "(: x Int)"

    it "declare (: f (-> A B))" $
      toString (Declare "f" [] [Atom "A"] $ Atom "B") `shouldBe`
      "(: f (-> A B))"

    it "declare (: f (-> A B C))" $
      toString (Declare "f" [] [Atom "A",Atom "B"] $ Atom "C") `shouldBe`
      "(: f (-> A B C))"

    it "declare (: f (: t A) (-> t t)))" $ do
      let premises = [Declare "t" [] [] (Atom "A")]
      let expr = Declare "f" premises [Atom "t"] $ Atom "t"
      toString expr `shouldBe` "(: f (: t A) (-> t t))"

  describe "currying function" $ do

    it "currying (\\ (x y) y) => (\\ x (\\ y y))" $ do
      let f = Func (Atom "x") [Atom "y"] [] (Atom "y")
      let g = Func (Atom "x") [] [] (Func (Atom "y") [] [] (Atom "y"))
      currying f `shouldBe` g
