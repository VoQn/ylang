module EvalSpec where

import Test.Hspec

import Data.Map as Map

import Ylang.Eval
import Ylang.Syntax

spec :: Spec
spec = do
  describe "eval" $ do
    it "evaluate integer value (..., -2, -1, 0, 1, 2, ...)" $
      fst (eval Map.empty (Int 1)) `shouldBe` (Int 1)

    it "evaluate float value (..., 0.2, -0.1, 0.0, 0.1, 0.2, ...)" $
      fst (eval Map.empty (Float 0.1)) `shouldBe` (Float 0.1)

    it "evaluate boolean value (Yes)" $
      fst (eval Map.empty (Boolean True)) `shouldBe` (Boolean True)

    it "evaluate boolean value (No)" $
      fst (eval Map.empty (Boolean False)) `shouldBe` (Boolean False)
