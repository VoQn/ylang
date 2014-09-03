module EvalSpec where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)

import Ylang.Eval
import Ylang.Value
import Ylang.Syntax


spec :: Spec
spec = do

  describe "evaluate atomic value" $ do
    prop "integer value (..., -2, -1, 0, 1, 2, ...)" $
      \ x -> do
        let exec = runEval1 defaultEnv1 $ eval1 $ Int x
        getResult1 exec `shouldReturn` Right (ValIntn x)

    prop "float value (..., 0.2, -0.1, 0.0, 0.1, 0.2, ...)" $
      \ x -> do
        let exec = runEval1 defaultEnv1 $ eval1 $ Float x
        getResult1 exec `shouldReturn` Right (ValFlon x)

    prop "rational value (..., -1/2, 1/1, -1/3, ...)" $
      \ x -> do
        let exec = runEval1 defaultEnv1 $ eval1 $ Ratio x
        getResult1 exec `shouldReturn` Right (ValRatn x)

    prop "boolean value (Yes / No)" $
      \ x -> do
        let exec = runEval1 defaultEnv1 $ eval1 $ Boolean x
        getResult1 exec `shouldReturn` Right (ValBool x)

  describe "evaluate sharrow definition assign" $ do

    it "assign (= x 10)" $ do
      let exec = runEval defaultEnv0 $ do
            r <- eval $ Define "x" (Int 10)
            return r
      getResult exec `shouldBe` Right (Define "x" (Int 10))

    it "assigned value vall (= x 10) (x)" $ do
      let exec = runEval defaultEnv0 $ do
            _ <- eval $ Define "x" (Int 10)
            r <- eval $ Atom "x"
            return r
      getResult exec `shouldBe` Right (Int 10)

    it "assigned function " $ do
      let exec = runEval defaultEnv0 $ do
            _ <- eval $ Define "id" $ Func (Atom "x") [] [] (Atom "x")
            r <- eval $ Atom "id"
            return r
      getResult exec `shouldBe`
        Right (Func (Atom "x") [] [] (Atom "x"))

  describe "not-defined value" $ do

    it "not-assigned X" $ do
      let exec = runEval defaultEnv0 $ do
            r <- eval $ Atom "undefined-value"
            return r
      getResult exec `shouldBe`
        Left "<Undefined Value> : undefined-value"

  describe "can't re-assign in that scope" $ do

    it "(= x 1); (= x 10)" $ do
      let exec = runEval defaultEnv0 $ do
            _ <- eval $ Define "x" $ Int 1
            r <- eval $ Define "x" $ Int 10
            return r
      getResult exec `shouldBe`
        Left (
         "<Conflict Definition>" ++
         " Already Defined :: (= x 1)" ++
         " But Reassigned :: (= x 10)"
        )
