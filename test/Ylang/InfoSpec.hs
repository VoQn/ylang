{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Ylang.InfoSpec where

import Control.Applicative

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Ylang.IO
import Ylang.Info

instance Arbitrary Info where
  arbitrary = oneof [pure Unknown, genFileInput]
    where
    genFileInput = FileInput <$> genFileName <*> genCount <*> genCount
    genFileName  = elements ["<stdin>", "test.ys"]
    genCount     = elements [1..1000]

spec :: Spec
spec = describe "Ylang Input information data-type" $ do

  describe "Info has File-Input Information" $ do
    let info = FileInput { fileName = "test.ys", line = 10, column = 20 }

    it "can get file-name" $
      fileName info `shouldBe` "test.ys"

    it "can get line-count" $
      line info `shouldBe` 10

    it "can get column-count" $
      column info `shouldBe` 20

  describe "as an instance of Eq type-class" $ do

    prop "A == B ==> B == A" $
      \(a, b :: Info) -> a == b `shouldBe` b == a

    prop "A /= B ==> B /= A" $
      \(a, b :: Info) -> a /= b `shouldBe` b /= a

  describe "as an instance of Show type-class" $

    prop "show (info :: Info)" $
      \(info :: Info) ->
        showList [info] `seq` shows info `seq` show info `seq` True

  describe "as an instance of Display type-class" $ do

    it "buildText Unknown" $
      buildText Unknown `shouldBe` "#unknown#"

    it "buildText FileInput { ... }" $ do
      let fi = FileInput { fileName = "test.ys", line = 1, column = 1 }
      buildText fi `shouldBe`
        "test.ys 1,1"
