{-# LANGUAGE OverloadedStrings #-}
module Ylang.IOSpec where

import Data.Text.Lazy.Builder (Builder)

import Test.Hspec

import Ylang.IO

spec :: Spec
spec = describe "YLang IO Interface" $ do

  describe "Display type-class" $

    it "can display IO" $
      display ("hello" :: String) `shouldReturn` ()

  describe "Lazy Text Builder is an instance of Display" $ do

    it "buildText <empty> => \"\"" $
      buildText ("" :: Builder) `shouldBe` ""

    it "buildText \"something text\" => \"something text\"" $
      buildText ("something text" :: Builder) `shouldBe`
        "something text"

  describe "Char is an instance of Display" $

    it "ASCII charactor" $
      buildText 'c' `shouldBe` "c"

  describe "Int is an instance of Display" $ do

    it "0" $ buildText (0 :: Int) `shouldBe` "0"

    it "-1" $ buildText ((-1) :: Int) `shouldBe` "-1"

    it "1" $ buildText (1 :: Int) `shouldBe` "1"

  describe "Integer is an instance of Display" $ do

    it "0" $ buildText (0 :: Integer) `shouldBe` "0"

    it "-1" $ buildText ((-1) :: Integer) `shouldBe` "-1"

    it "1" $ buildText (1 :: Integer) `shouldBe` "1"

    it "9999999999999999999999999999999999999999" $ do
      let bigInt = 9999999999999999999999999999999999999999 :: Integer
      buildText bigInt `shouldBe`
        "9999999999999999999999999999999999999999"

    it "-9999999999999999999999999999999999999999" $ do
      let bigNegative = (-9999999999999999999999999999999999999999) :: Integer
      buildText bigNegative `shouldBe`
        "-9999999999999999999999999999999999999999"

  describe "text-build convinators" $ do

    it "parens x -> \"(x)\"" $
      parens "x" `shouldBe` "(x)"

    it "sep \":\" \"\" => \"\"" $
      (sep ":" [] :: Builder) `shouldBe` ""

    it "sep \",\" $ map buildText [1..5] => \"1,2,3,4,5\"" $
      sep "," (map buildText ([1..5] :: [Int])) `shouldBe`
        "1,2,3,4,5"

    it "spaces $ map buildText [1..5] => \"1 2 3 4 5\"" $
      spaces (map buildText ([1..5] :: [Int])) `shouldBe`
        "1 2 3 4 5"
