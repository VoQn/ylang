{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Ylang.Syntax.LiteralSpec where

import Test.Hspec
import Ylang.IO
import Ylang.Syntax.Literal

spec :: Spec
spec = describe "Ylang Literal" $

  describe "as an instance of Display" $

    it "buildText Yes => \"Yes\"" $
      buildText (LitBool True) `shouldBe` "Yes"
