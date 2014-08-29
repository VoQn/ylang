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
        runEval (eval defaultEnv (Int x)) `shouldBe` Right (Int x)

    it "float value (..., 0.2, -0.1, 0.0, 0.1, 0.2, ...)" $
      property $ \ x ->
        runEval (eval defaultEnv (Float x)) `shouldBe` Right (Float x)

    it "rational value (..., -1/2, 1/1, -1/3, ...)" $
      property $ \ x ->
        runEval (eval defaultEnv (Ratio x)) `shouldBe` Right (Ratio x)

    it "boolean value (Yes / No)" $
      property $ \ x ->
        runEval (eval defaultEnv (Boolean x)) `shouldBe` Right (Boolean x)

  describe "evaluate sharrow definition assign" $ do

    it "assign (= x 10)" $ do
      let def = Define "x" (Int 10)
      runEval (eval defaultEnv def) `shouldBe` Right (Atom "x")
{-
    it "assigned value vall" $ do
      let assignedEnv = eval defaultEnv fact
      t <- eval assignedEnv (Atom "x")
      y `shouldBe` Int 10
-}
