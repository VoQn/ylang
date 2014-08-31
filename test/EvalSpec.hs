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
        let exec = runEval defaultEnv $ eval $ Int x
        getResult exec `shouldBe` Right (Int x)

    it "float value (..., 0.2, -0.1, 0.0, 0.1, 0.2, ...)" $
      property $ \ x -> do
        let exec = runEval defaultEnv $ eval $ Float x
        getResult exec `shouldBe` Right (Float x)

    it "rational value (..., -1/2, 1/1, -1/3, ...)" $
      property $ \ x -> do
        let exec = runEval defaultEnv $ eval $ Ratio x
        getResult exec `shouldBe` Right (Ratio x)

    it "boolean value (Yes / No)" $
      property $ \ x -> do
        let exec = runEval defaultEnv $ eval $ Boolean x
        getResult exec `shouldBe` Right (Boolean x)

  describe "evaluate sharrow definition assign" $ do

    it "assign (= x 10)" $ do
      let defv = Define "x" (Int 10)
      let exec = runEval defaultEnv $ eval defv
      getResult exec `shouldBe` Right defv

    it "assigned value vall (= x 10) (x)" $ do
      let defv = Define "x" (Int 10)
      let env' = getEnv $ runEval defaultEnv $ eval defv
      let exam = runEval env' $ eval $ Atom "x"
      getResult exam `shouldBe` Right (Int 10)

    it "assigned function " $ do
      let func = Func (Atom "x") [] [] (Atom "x")
      let defn = Define "id" $ func
      let env' = getEnv $ runEval defaultEnv $ eval defn
      let exam = runEval env' $ eval $ Atom "id"
      getResult exam `shouldBe` Right func
