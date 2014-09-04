{-# LANGUAGE OverloadedStrings #-}
module ValueSpec where

import Data.Ratio ((%))

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Ylang.Type
import Ylang.Value
import Ylang.Display

spec :: Spec
spec = do

  describe "ylang value should have own type" $ do

    it "Bottom (_|_)" $
      getType ValBotm `shouldBe` TySet

    it "Unit ()" $
      getType ValUnit `shouldBe` TyUnit

    prop "Bool (yes|no)" $ \b ->
      getType (ValBool b) `shouldBe` TyBool

    prop "Keyword (:keyword)" $ \k ->
      getType (ValKeyw k) `shouldBe` TyKeyw

    prop "Integer (... -1, 0, 1, ...)" $ \i ->
      getType (ValIntn i) `shouldBe` TyIntn

    prop "Flonum (-inf.0, ... -1.0, -0.5, 0.0, 0.5, 1.0 ... +inf.0)" $ \f ->
      getType (ValFlon f) `shouldBe` TyFlon

    prop "Rational (... -1/2, -1/3, 0/1, 1/1, 1/3, 1/2 ...)" $ \r ->
      getType (ValRatn r) `shouldBe` TyRatn

    prop "Charactor ('a', 'b', 'c', ...)" $ \c ->
      getType (ValChr c) `shouldBe` TyChr

    prop "String (\"foo\", \"bar\", \"baz\" ...)" $ \s ->
      getType (ValStr s) `shouldBe` TyStr

    prop "Pair (, x y)" $ \x y ->
      getType (ValPair (ValIntn x) (ValBool y)) `shouldBe`
      TyPair TyIntn TyBool

  describe "ylang value is instance of Display type class" $ do

    it "Bottom (_|_)" $
      toString ValBotm `shouldBe` "_|_"

    it "Unit ()" $
      toString ValUnit `shouldBe` "()"

    it "Bool (yes)" $
      toString (ValBool True) `shouldBe` "yes"

    it "Bool (no)" $
      toString (ValBool False) `shouldBe` "no"

    it "Keyword :key" $
      toString (ValKeyw "key") `shouldBe` ":key"

    it "Integer (1)" $
      toString (ValIntn 1) `shouldBe` "1"

    it "Flonum (0.3)" $
      toString (ValFlon 0.3) `shouldBe` "0.3"

    it "Flonum (0.0)" $
      toString (ValFlon 0.0) `shouldBe` "0.0"

    it "Rational (1/3)" $
      toString (ValRatn $ 1%3) `shouldBe` "1/3"

    it "Rational (3/3)" $
      toString (ValRatn $ 3%3) `shouldBe` "1"

    it "Charactor 'c'" $
      toString (ValChr 'c') `shouldBe` "'c'"

    it "String \"foo\"" $
      toString (ValStr "foo") `shouldBe` "\"foo\""

    it "Pair (1, yes)" $
      toString (ValPair (ValIntn 1) (ValBool True)) `shouldBe`
      "(, 1 yes)"

    it "Array []" $
      toString (ValArray []) `shouldBe` "[]"

    it "Array [1 2 3]" $
      toString (ValArray [ValIntn 1,ValIntn 2,ValIntn 3]) `shouldBe`
      "[1 2 3]"

    it "var x (= x 10)" $
      toString (ValVar "x" TyIntn (ValIntn 10)) `shouldBe`
      "(= x 10)"
