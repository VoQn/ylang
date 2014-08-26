module TypeSpec where

import Test.Hspec

import Ylang.Type

spec :: Spec
spec = do
  describe "show ylang type" $ do

    it "Bool type" $
      show TyBool `shouldBe` "Bool"

    it "Integer type" $
      show TyInt `shouldBe` "Int"

    it "Alpha type" $
      (show $ TyVar 1) `shouldBe` "a1"

    it "(Bool -> Bool) type" $
      (show $ TyFunc TyBool TyBool)
      `shouldBe` "Bool -> Bool"

    it "Arrow (Bool -> Bool -> Bool)" $
      (show $ TyFunc TyBool (TyFunc TyBool TyBool))
      `shouldBe` "Bool -> Bool -> Bool"
