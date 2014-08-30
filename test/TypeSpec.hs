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
      show TyIntn `shouldBe` "Integer"

    it "Rational number type" $
      show TyRatn `shouldBe` "Ratio"

    it "Floating point number type" $
      show TyFlon `shouldBe` "Flonum"

    it "Keyword type" $
      show TyKeyw `shouldBe` "Keyword"

    it "Charactor type" $
      show TyChar `shouldBe` "Char"

    it "String type" $
      show TyStr `shouldBe` "String"

    it "String type" $
      show TyRope `shouldBe` "Rope"

    it "Alpha type" $
      (show $ TyVar "a" 1) `shouldBe` "a1"

    it "(-> Bool Bool) type" $
      (show $ TyFunc TyBool TyBool) `shouldBe` "(-> Bool Bool)"

    it "Arrow (-> Bool (-> Bool Bool))" $
      (show $ TyFunc TyBool (TyFunc TyBool TyBool))
      `shouldBe` "(-> Bool (-> Bool Bool))"
