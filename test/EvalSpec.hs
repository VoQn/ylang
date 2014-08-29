module EvalSpec where

import Test.Hspec
import Test.QuickCheck (property)

import Ylang.Eval
import Ylang.Syntax


spec :: Spec
spec = do

  describe "evaluate atomic value" $ do
    it "integer value (..., -2, -1, 0, 1, 2, ...)" $
      property $ \ x -> do
        y <- eval defaultEnv (Int x)
        y `shouldBe` Int x

    it "float value (..., 0.2, -0.1, 0.0, 0.1, 0.2, ...)" $
      property $ \ x -> do
        y <- eval defaultEnv (Float x)
        y `shouldBe` Float x

    it "rational value (..., -1/2, 1/1, -1/3, ...)" $
      property $ \ x -> do
        y <- eval defaultEnv (Ratio x)
        y `shouldBe` Ratio x

    it "boolean value (Yes / No)" $
      property $ \ x -> do
        y <- eval defaultEnv (Boolean x)
        y `shouldBe` Boolean x

  describe "evaluate sharrow definition assign" $ do

    it "assign (= x 10)" $ do
      let def = Define "x" (Int 10)
      y <- eval defaultEnv def
      y `shouldBe` Atom "x"
{-
    it "assigned value vall" $ do
      let assignedEnv = eval defaultEnv fact
      t <- eval assignedEnv (Atom "x")
      y `shouldBe` Int 10
-}
