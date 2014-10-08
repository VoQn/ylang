{-# LANGUAGE OverloadedStrings #-}
module Ylang.TypeSpec where

import Test.Hspec
import Ylang.IO
import Ylang.Type

spec :: Spec
spec = describe "Ylang Type" $
  describe "as an instance of Display" $ do

    it "Top : Set" $
      buildText TyTop `shouldBe` "Set"

    it "Bottom : _|_" $
      buildText TyBottom `shouldBe` "_|_"

    it "Unit : ()" $
      buildText TyUnit `shouldBe` "()"

    it "Bool : Bool" $
      buildText TyBool `shouldBe` "Bool"

    it "Char : Char" $
      buildText TyChar `shouldBe` "Char"

    it "String : String" $
      buildText TyString `shouldBe` "String"

    it "Keyword : Keyword" $
      buildText TyKeyword `shouldBe` "Keyword"

    it "Nat : Nat" $
      buildText TyNatural `shouldBe` "Nat"

    it "Integer : Integer" $
      buildText TyInteger `shouldBe` "Integer"

    it "Flonum : Flonum" $
      buildText TyFlonum `shouldBe` "Flonum"

    it "Ratio : Rational" $
      buildText TyRatio `shouldBe` "Rational"
