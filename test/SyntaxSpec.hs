{-# LANGUAGE OverloadedStrings #-}

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

    it "charactor (c) == charactor (c)" $
      property $ \ c -> Char c == Char c

  describe "ylang expression type can show own" $ do
    it "boolean (yes)" $
      toText (Boolean True) `shouldBe` "yes"

    it "boolean (no)" $
      toText (Boolean False) `shouldBe` "no"

    it "integer 0" $
      toText (Int 0) `shouldBe` "0"

    it "integer 10" $
      toText (Int 10) `shouldBe` "10"

    it "floating 0.5" $
      toText (Float 0.5) `shouldBe` "0.5"

    it "rational 1/10" $
      toText (Ratio (1 % 10)) `shouldBe` "1/10"

    it "charactor 'c'" $
      toText (Char 'c') `shouldBe` "'c'"

    it "string \"\"" $
      toText (String "") `shouldBe` "\"\""

    it "string \"message\"" $
      toText (String "message") `shouldBe` "\"message\""

    it "keyword :key" $
      toText (Keyword "key") `shouldBe` ":key"

    it "operator (&)" $
      toText (Atom "&") `shouldBe` "&"

    it "variable x" $
      toText (Atom "x") `shouldBe` "x"

    it "pair (, x y)" $
      toText (Pair (Atom "x") (Atom "y")) `shouldBe` "(, x y)"

    it "list []" $
      toText (Array []) `shouldBe` "[]"

    it "list [1 2 3]" $
      toText (Array [Int 1,Int 2,Int 3]) `shouldBe` "[1 2 3]"

    it "void ()" $
      toText Void `shouldBe` "()"

    it "func (\\ x x)" $
      toText (Func (Atom "x") [] [] (Atom "x")) `shouldBe` "(\\ x x)"

    it "func (\\ (x y) y)" $
      toText (Func (Atom "x") [Atom "y"] [] (Atom "y")) `shouldBe`
      "(\\ (x y) y)"

    it "func (\\ (x y) (= z (+ x y)) z)" $ do
      let g = Define "z" $ Call (Atom "+") [Atom "x",Atom "y"]
      let f = Func (Atom "x") [Atom "y"] [g] $ Atom "z"
      toText f `shouldBe` "(\\ (x y) (= z (+ x y)) z)"

    it "apply (f x y)" $
      toText (Factor [Atom "f",Atom "x",Atom "y"]) `shouldBe`
      "(f x y)"

    it "arrow (-> A B)" $
      toText (Arrow (Atom "A") [] (Atom "B")) `shouldBe`
      "(-> A B)"

    it "arrow (-> A B C)" $
      toText (Arrow (Atom "A") [Atom "B"] (Atom "C")) `shouldBe`
      "(-> A B C)"

    it "arrow (-> (A B) C)" $
      toText (Arrow (Factor [Atom "A",Atom "B"]) [] (Atom "C")) `shouldBe`
      "(-> (A B) C)"

    it "define (= x 10)" $
      toText (Define "x" $ Int 10) `shouldBe`
      "(= x 10)"

    it "define (= (f x) x)" $
      toText (Define "f" $ Func (Atom "x") [] [] (Atom "x")) `shouldBe`
      "(= (f x) x)"

    it "define (= (f x) (\\ z (+ z 1)) (z x))" $ do
      let h = Func (Atom "z") [] [] $ Call (Atom "+") [Atom "z", Int 1]
      let g f = Func (Atom "x") [] [f] $ Call (Atom "z") [Atom "x"]
      toText (Define "f" $ g h) `shouldBe` "(= (f x) (\\ z (+ z 1)) (z x))"

    it "declare (: x Int)" $
      toText (Declare "x" [] [] $ Atom "Int") `shouldBe`
      "(: x Int)"

    it "declare (: f (-> A B))" $
      toText (Declare "f" [] [Atom "A"] $ Atom "B") `shouldBe`
      "(: f (-> A B))"

    it "declare (: f (-> A B C))" $
      toText (Declare "f" [] [Atom "A",Atom "B"] $ Atom "C") `shouldBe`
      "(: f (-> A B C))"

    it "declare (: f (: t A) (-> t t)))" $ do
      let premises = [Declare "t" [] [] (Atom "A")]
      let expr = Declare "f" premises [Atom "t"] $ Atom "t"
      toText expr `shouldBe` "(: f (: t A) (-> t t))"

  describe "currying function" $ do

    it "currying (\\ (x y) y) => (\\ x (\\ y y))" $ do
      let f = Func (Atom "x") [Atom "y"] [] (Atom "y")
      let g = Func (Atom "x") [] [] (Func (Atom "y") [] [] (Atom "y"))
      currying f `shouldBe` g
