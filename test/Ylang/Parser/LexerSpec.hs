{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Ylang.Parser.LexerSpec where

import           Data.Ratio
import           Data.Text            (Text)
import           Test.Hspec
import           Text.Parsec
import           Text.Parsec.Error
import           Text.Parsec.Text     (Parser)
import           Ylang.Parser.Lexer
import           Ylang.Syntax.Literal

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
    testParse tIntn "-10" `shouldBe` Right (LitIntn $ -10)

  it "parse \"0.1\" => 0.1" $
    testParse tFlon "0.1" `shouldBe` Right (LitFlon 0.1)

  it "parse \"-0.1\" => -0.1" $
    testParse tFlon "-0.1" `shouldBe` Right (LitFlon $ -0.1)

  it "parse \"1/2\" => 1/2" $
    testParse tRatn "1/2" `shouldBe` Right (LitRatn $ 1 % 2)

  it "parse \"-1/10\" => -1/10" $
    testParse tRatn "-1/10" `shouldBe` Right (LitRatn $ -1 % 10)

  it "parse \"'\\\''\" => \"'\\\''\"" $
    testParse tChr "'\''" `shouldBe` Right (LitChr '\'')

  it "parse \"\"\"\" => \"\"" $
    testParse tStr "\"\"" `shouldBe` Right (LitStr "")
