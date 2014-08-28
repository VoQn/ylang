module EvalSpec where

import Test.Hspec

import Data.Map as Map

import Ylang.Eval
import Ylang.Syntax

spec :: Spec
spec = do
  describe "evaluate atomic value" $ do
    it "integer value (..., -2, -1, 0, 1, 2, ...)" $
      snd (eval defaultEnv (Int 1)) `shouldBe` (Int 1)

    it "float value (..., 0.2, -0.1, 0.0, 0.1, 0.2, ...)" $
      snd (eval defaultEnv (Float 0.1)) `shouldBe` (Float 0.1)

    it "boolean value (Yes)" $
      snd (eval defaultEnv (Boolean True)) `shouldBe` (Boolean True)

    it "boolean value (No)" $
      snd (eval defaultEnv (Boolean False)) `shouldBe` (Boolean False)
