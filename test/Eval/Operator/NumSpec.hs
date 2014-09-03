module Eval.Operator.NumSpec where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck ((==>))
import Ylang.Eval
import Ylang.Value
import Ylang.Syntax

spec :: Spec
spec = do
  describe "Primitive Add Operation (+)" $ do

    it "(+ 1 2)" $ do
      let exec = runEval1 defaultEnv1 $ do
            return =<< eval1 $ Call (Atom "+") [Int 1, Int 2]
      getResult1 exec `shouldReturn` Right (ValIntn 3)

    it "(+ 1 2 3 4)" $ do
      let exec = runEval1 defaultEnv1 $ do
            return =<< eval1 $ Call (Atom "+") [Int 1, Int 2, Int 3, Int 4]
      getResult1 exec `shouldReturn` Right (ValIntn 10)

    prop "(+ x y z ...)" $ \ xs ->
      length xs > 1 ==> do
        let exec = runEval1 defaultEnv1 $ do
              return =<< eval1 $ Call (Atom "+") $ map Int xs
        getResult1 exec `shouldReturn` Right (ValIntn (foldl (+) 0 xs))
