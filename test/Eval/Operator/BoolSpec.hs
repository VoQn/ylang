module Eval.Operator.BoolSpec where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck ((==>))
import Ylang.Eval
import Ylang.Value
import Ylang.Syntax

spec :: Spec
spec = do
  describe "Primitive And Operation (&)" $ do

    it "(& no no) -> (no)" $ do
      let exec = runEval1 defaultEnv1 $ do
            r <- eval1 $ Call (Atom "&") [Boolean False, Boolean False]
            return r
      getResult1 exec `shouldReturn` Right (ValBool False)

    it "(& yes no) -> (no)" $ do
      let exec = runEval1 defaultEnv1 $ do
            r <- eval1 $ Call (Atom "&") [Boolean True, Boolean False]
            return r
      getResult1 exec `shouldReturn` Right (ValBool False)

    it "(& no yes) -> (no)" $ do
      let exec = runEval1 defaultEnv1 $ do
            r <- eval1 $ Call (Atom "&") [Boolean False, Boolean True]
            return r
      getResult1 exec `shouldReturn` Right (ValBool False)

    it "(& yes yes) -> (yes)" $ do
      let exec = runEval1 defaultEnv1 $ do
            r <- eval1 $ Call (Atom "&") [Boolean True, Boolean True]
            return r
      getResult1 exec `shouldReturn` Right (ValBool True)

    prop "(& x y z ...)" $ \ xs ->
      (length xs) > 1 ==> do
        let exec = runEval1 defaultEnv1 $ do
              return =<< eval1 $ Call (Atom "&") $ map Boolean xs
        getResult1 exec `shouldReturn` Right (ValBool (foldl (&&) True xs))

  describe "Primitve Or Operation (|)" $ do

    it "(| no no) -> (no)" $ do
      let exec = runEval1 defaultEnv1 $ do
            r <- eval1 $ Call (Atom "|") [Boolean False, Boolean False]
            return r
      getResult1 exec `shouldReturn` Right (ValBool False)

    it "(| yes no) -> (yes)" $ do
      let exec = runEval1 defaultEnv1 $ do
            r <- eval1 $ Call (Atom "|") [Boolean True, Boolean False]
            return r
      getResult1 exec `shouldReturn` Right (ValBool True)

    it "(| no yes) -> (no)" $ do
      let exec = runEval1 defaultEnv1 $ do
            r <- eval1 $ Call (Atom "|") [Boolean False, Boolean True]
            return r
      getResult1 exec `shouldReturn` Right (ValBool True)

    it "(| yes yes) -> (yes)" $ do
      let exec = runEval1 defaultEnv1 $ do
            r <- eval1 $ Call (Atom "|") [Boolean True, Boolean True]
            return r
      getResult1 exec `shouldReturn` Right (ValBool True)

    prop "(| x y z ...)" $ \ xs ->
      (length xs) > 1 ==> do
        let exec = runEval1 defaultEnv1 $ do
              return =<< eval1 $ Call (Atom "|") $ map Boolean xs
        getResult1 exec `shouldReturn` Right (ValBool (foldl (||) False xs))

  describe "Primitive Not Operation (~)" $ do

    it "(~ no) -> (yes)" $ do
      let exec = runEval1 defaultEnv1 $ do
            r <- eval1 $ Call (Atom "~") [Boolean False]
            return r
      getResult1 exec `shouldReturn` Right (ValBool True)

    it "(~ yes) -> (no)" $ do
      let exec = runEval1 defaultEnv1 $ do
            r <- eval1 $ Call (Atom "~") [Boolean True]
            return r
      getResult1 exec `shouldReturn` Right (ValBool False)

    it "(~ no no) -> (no)" $ do
      let exec = runEval1 defaultEnv1 $ do
            r <- eval1 $ Call (Atom "~") [Boolean False, Boolean False]
            return r
      getResult1 exec `shouldReturn` Left "Too Parameter"
