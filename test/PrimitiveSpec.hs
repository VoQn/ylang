module PrimitiveSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Ylang.Value
import Ylang.Primitive

shouldBeRight :: Either String Val -> Val -> Expectation
shouldBeRight x y = x `shouldBe` Right y

spec :: Spec
spec = do
  describe "primitive Arithmetic calcuation" $ do
    prop "(: (+ x y) Integer)" $ \ x y ->
      (ValIntn x) `addBin` (ValIntn y) `shouldBeRight` ValIntn (x + y)

    prop "(: (+ x y) Flonum)" $ \ x y ->
      (ValFlon x) `addBin` (ValFlon y) `shouldBeRight` ValFlon (x + y)

    prop "(: (+ x y) Ratio)" $ \ x y ->
      (ValRatn x) `addBin` (ValRatn y) `shouldBeRight` ValRatn (x + y)

    prop "(: (+ x y z ...) Integer)" $ \ xs ->
      xs /= [] ==>
      adds (map ValIntn xs) `shouldBeRight` ValIntn (foldl (+) 0 xs)

    prop "(: (+ x y z ...) Flonum)" $ \ xs ->
      xs /= [] ==>
      adds (map ValFlon xs) `shouldBeRight` ValFlon (foldl (+) 0 xs)

    prop "(: (+ x y z ...) Ratio)" $ \ xs ->
      xs /= [] ==>
      adds (map ValRatn xs) `shouldBeRight` ValRatn (foldl (+) 0 xs)
