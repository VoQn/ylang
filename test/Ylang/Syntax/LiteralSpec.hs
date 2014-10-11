{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Ylang.Syntax.LiteralSpec where

import Control.Applicative
import Data.Text.Gen()
import Data.Ratio
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Ylang.IO
import Ylang.Type
import Ylang.Syntax.Literal

instance Arbitrary Lit where
  arbitrary = oneof [
      pure LitHole
    , pure LitUnit
    , LitBool <$> arbitrary
    , LitChr  <$> arbitrary
    , LitStr  <$> arbitrary
    , LitKey  <$> arbitrary
    , LitIntn <$> arbitrary
    , LitFlon <$> arbitrary
    , LitRatn <$> arbitrary
    ]

isFractional :: Lit -> Bool
isFractional (LitFlon _) = True
isFractional _ = False

spec :: Spec
spec = describe "Ylang Literal" $ do

  describe "as an instance of Eq type-class" $ do

    prop "A == B ==> B == A" $
      \(a, b :: Lit) ->
        (not (isFractional a) && not (isFractional b)) ==>
        a == b `shouldBe` b == a

    prop "A /= B ==> B /= A" $
      \(a, b :: Lit) ->
        (not (isFractional a) && not (isFractional b)) ==>
        a /= b `shouldBe` b /= a

  describe "as an instance of Show type-class" $

    prop "show (info :: Info)" $
      \(info :: Lit) ->
        showList [info] `seq` shows info `seq` show info `seq` True

  describe "as an instance of Display" $ do

    it "buildText _ => \"_\"" $
      buildText LitHole `shouldBe` "_"

    it "buildText () => \"()\"" $
      buildText LitUnit `shouldBe` "()"

    it "buildText Yes => \"Yes\"" $
      buildText (LitBool True) `shouldBe` "Yes"

    it "buildText No => \"No\"" $
      buildText (LitBool False) `shouldBe` "No"

    it "buildText 'c' => \"'c'\"" $
      buildText (LitChr 'c') `shouldBe` "'c'"

    it "buildText \"a text\" => \"\\\"a text\\\"\"" $
      buildText (LitStr "a text") `shouldBe` "\"a text\""

    it "buildText :keyword => \":keyword\"" $
      buildText (LitKey "keyword") `shouldBe` ":keyword"

    it "buildText 0 => \"0\"" $
      buildText (LitIntn 0) `shouldBe` "0"

    it "buildText 1 => \"1\"" $
      buildText (LitIntn 1) `shouldBe` "1"

    it "buildText -1 => \"-1\"" $
      buildText (LitIntn (-1)) `shouldBe` "-1"

    it "buildText 0.0 => \"0.0\"" $
      buildText (LitFlon 0) `shouldBe` "0.0"

    it "buildText 0.1 => \"0.1\"" $
      buildText (LitFlon 0.1) `shouldBe` "0.1"

    it "buildText -0.5 => \"-0.5\"" $
      buildText (LitFlon (-0.5)) `shouldBe` "-0.5"

    it "buildText 0/1 => \"0/1\"" $
      buildText (LitRatn 0) `shouldBe` "0/1"

    it "buildText 1/1 => \"1/1\"" $
      buildText (LitRatn 1) `shouldBe` "1/1"

    it "buildText -1/1 => \"-1/1\"" $
      buildText (LitRatn (-1)) `shouldBe` "-1/1"

    it "buildText 1/2 => \"1/2\"" $
      buildText (LitRatn (1 % 2)) `shouldBe` "1/2"

  describe "Enable get own type" $ do

    it "Hole (_)" $
      typeofLit LitHole `shouldBe` TyBottom

    it "Unit ()" $
      typeofLit LitUnit `shouldBe` TyUnit

    prop "Bool (Yes/No)" $
      \b -> typeofLit (LitBool b) `shouldBe` TyBool

    prop "Char {'a','b','0', ... }" $
      \c -> typeofLit (LitChr c) `shouldBe` TyChar

    prop "String {\"\",\"foo\",\"bar\", ...}" $
      \str -> typeofLit (LitStr str) `shouldBe` TyString

    prop "Keyword {:a, :b, :c, ... }" $
      \k -> typeofLit (LitKey k) `shouldBe` TyKeyword

    prop "Integer {... , -1, 0, 1, ... }" $
      \i -> typeofLit (LitIntn i) `shouldBe` TyInteger

    prop "Flonum {... , -0.1, 0.0, 0.1 , ...}" $
      \f -> typeofLit (LitFlon f) `shouldBe` TyFlonum

    prop "Ratio {..., -1/10, 0/1, 1/10, ...}" $
      \r -> typeofLit (LitRatn r) `shouldBe` TyRatio
