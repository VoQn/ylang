module EvalSpec where

import Test.Hspec
import Test.QuickCheck (property)

import Ylang.Eval
import Ylang.Syntax

spec :: Spec
spec = do
  describe "evaluate atomic value" $ do
    it "integer value (..., -2, -1, 0, 1, 2, ...)" $
      property $ \ x ->
        snd (eval defaultEnv $ Int x) `shouldBe` Int x

    it "float value (..., 0.2, -0.1, 0.0, 0.1, 0.2, ...)" $
      property $ \ x ->
        snd (eval defaultEnv $ Float x) `shouldBe` Float x

    it "rational value (..., -1/2, 1/1, -1/3, ...)" $
      property $ \ x ->
        snd (eval defaultEnv $ Ratio x) `shouldBe` Ratio x

    it "boolean value (Yes / No)" $
      property $ \ x ->
        snd (eval defaultEnv $ Boolean x) `shouldBe` Boolean x

  describe "evaluate sharrow definition assign" $ do

    let fact = Factor [Atom "=",Atom "x",Int 10]
    it "assign (= x 10)" $
      snd (eval defaultEnv fact) `shouldBe` Atom "x"

    let assignedEnv = fst $ eval defaultEnv fact
    it "assigned value vall" $
      snd (eval assignedEnv $ Atom "x") `shouldBe` Int 10
