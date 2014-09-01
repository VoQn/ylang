module TypeSpec where

import Test.Hspec

import Ylang.Type
import Ylang.Display (toString)

spec :: Spec
spec = do
  describe "show ylang type" $ do

    it "Unit type" $
      toString TyUnit `shouldBe` "()"

    it "Set type" $
      toString TySet `shouldBe` "Set"

    it "Boolean type" $
      toString TyBool `shouldBe` "Bool"

    it "Integer number type" $
      toString TyIntn `shouldBe` "Integer"

    it "Rational number type" $
      toString TyRatn `shouldBe` "Ratio"

    it "Floating point number type" $
      toString TyFlon `shouldBe` "Flonum"

    it "Keyword type" $
      toString TyKeyw `shouldBe` "Keyword"

    it "Charactor type" $
      toString TyChar `shouldBe` "Char"

    it "String type" $
      toString TyStr `shouldBe` "String"

    it "String type" $
      toString TyRope `shouldBe` "Rope"

    it "Alpha type" $
      (toString $ TyVar "a" 1) `shouldBe` "a1"

    it "(-> Bool Bool) type" $
      (toString $ TyFunc TyBool TyBool) `shouldBe` "(-> Bool Bool)"

    it "Arrow (-> Bool (-> Bool Bool))" $
      (toString $ TyFunc TyBool (TyFunc TyBool TyBool))
      `shouldBe` "(-> Bool (-> Bool Bool))"
