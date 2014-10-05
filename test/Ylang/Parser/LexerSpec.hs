{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Ylang.Parser.LexerSpec where

import Data.Text (Text)
import Text.Parsec
import Text.Parsec.Text (Parser)
import Text.Parsec.Error
import Test.Hspec
import Ylang.Syntax.Literal
import Ylang.Parser.Lexer

instance Eq ParseError where
  a == b = errorMessages a == errorMessages b

testParse :: Parser a -> Text -> Either ParseError a
testParse p = parse p "<test>"

spec :: Spec
spec = describe "Ylang Lexer" $ do

  it "parse \"Yes\" => yes" $
    testParse tBool "Yes" `shouldBe` Right (LitBool True)

  it "parse \"No\" => no" $
    testParse tBool "No" `shouldBe` Right (LitBool False)

  it "parse \"0\" => 0" $
    testParse tIntn "0" `shouldBe` Right (LitIntn 0)

  it "parse \"-10\" => -10" $
    testParse tIntn "-10" `shouldBe` Right (LitIntn (-10))
  it "parse \"0.1\" => 0.1" $
    testParse tFlon "0.1" `shouldBe` Right (LitFlon 0.1)
