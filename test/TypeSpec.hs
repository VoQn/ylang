module TypeSpec where

import Test.Hspec

import Ylang.Type

spec :: Spec
spec = do
  describe "show ylang type" $ do
    it "Set type" $
      show TySet `shouldBe` "Set"

    it "Boolean type" $
      show TyBool `shouldBe` "Bool"

    it "Integer number type" $
      show TyInt `shouldBe` "Int"

    it "Rational number type" $
      show TyRatio `shouldBe` "Ratio"

    it "Floating point number type" $
      show TyFloat `shouldBe` "Float"

    it "Keyword type" $
      show TyKeyword `shouldBe` "Keyword"

    it "Charactor type" $
      show TyChar `shouldBe` "Char"

    it "String type" $
      show TyString `shouldBe` "String"

    it "Alpha type" $
      (show $ TyVar "a" 1) `shouldBe` "a1"

    it "(-> Bool Bool) type" $
      (show $ TyFunc TyBool TyBool) `shouldBe` "(-> Bool Bool)"

    it "Arrow (-> Bool (-> Bool Bool))" $
      (show $ TyFunc TyBool (TyFunc TyBool TyBool))
      `shouldBe` "(-> Bool (-> Bool Bool))"
