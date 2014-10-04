{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Ylang.Syntax.LiteralSpec where

import Test.Hspec
import Ylang.IO
import Ylang.Syntax.Literal

spec :: Spec
spec = describe "Ylang Literal" $

  describe "as an instance of Display" $ do

    it "buildText Yes => \"Yes\"" $
      buildText (LitBool True) `shouldBe` "Yes"

    it "buildText No => \"No\"" $
      buildText (LitBool False) `shouldBe` "No"

    it "buildText 'c' => \"'c'\"" $
      buildText (LitChr 'c') `shouldBe` "'c'"

    it "buildText \"a text\" => \"\"a text\"\"" $
      buildText (LitStr "a text") `shouldBe` "\"a text\""

    it "buildText :keyword => \":keyword\"" $
      buildText (LitKey "keyword") `shouldBe` ":keyword"
