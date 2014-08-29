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
        fst (runEval defaultEnv (eval (Int x))) `shouldBe` Right (Int x)

    it "float value (..., 0.2, -0.1, 0.0, 0.1, 0.2, ...)" $
      property $ \ x ->
        fst (runEval defaultEnv (eval (Float x))) `shouldBe` Right (Float x)

    it "rational value (..., -1/2, 1/1, -1/3, ...)" $
      property $ \ x ->
        fst (runEval defaultEnv (eval (Ratio x))) `shouldBe` Right (Ratio x)

    it "boolean value (Yes / No)" $
      property $ \ x ->
        fst (runEval defaultEnv (eval (Boolean x))) `shouldBe` Right (Boolean x)

  describe "evaluate sharrow definition assign" $ do

    it "assign (= x 10)" $ do
      let def = Define "x" (Int 10)
      fst (runEval defaultEnv (eval def)) `shouldBe` Right (Atom "x")

    it "assigned value vall (= x 10) (x)" $ do
      let def = Define "x" (Int 10)
      let assignedEnv = snd (runEval defaultEnv (eval def))
      fst (runEval assignedEnv (eval (Atom "x"))) `shouldBe` Right (Int 10)

    it "assigned function " $ do
      let def = Define "id" $ Func (Atom "x") [] [] (Atom "x")
      let assignedEnv = snd (runEval defaultEnv (eval def))
      fst (runEval assignedEnv (eval (Atom "id"))) `shouldBe` Right (Func (Atom "x") [] [] (Atom "x"))
