{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Ylang.Parser.TokenSpec where

import Test.Hspec

import Data.Ratio        ((%))
import Data.Text         (Text)
import Text.Parsec       (ParseError, parse)
import Text.Parsec.Text  (Parser)
import Text.Parsec.Error (errorMessages)

import Ylang.Syntax.Literal (Lit(..))
import Ylang.Parser.Token

instance Eq ParseError where
  a == b = errorMessages a == errorMessages b

testParse :: Parser a -> Text -> Either ParseError a
testParse p = parse p "<test>"

spec :: Spec
spec = describe "Ylang Lexer" $ do

  it "parse \"_\" => _" $
    testParse hole "_" `shouldBe` Right LitHole

  it "parse \"()\" => ()" $
    testParse unit "()" `shouldBe` Right LitUnit

  it "parse \"Yes\" => yes" $
    testParse boolean "Yes" `shouldBe` Right (LitBool True)

  it "parse \"No\" => no" $
    testParse boolean "No" `shouldBe` Right (LitBool False)

  it "parse \"0\" => 0" $
    testParse integer "0" `shouldBe` Right (LitIntn 0)

  it "parse \"-10\" => -10" $
    testParse integer "-10" `shouldBe` Right (LitIntn (-10))

  it "parse \"0.1\" => 0.1" $
    testParse float "0.1" `shouldBe` Right (LitFlon 0.1)

  it "parse \"-0.1\" => -0.1" $
    testParse float "-0.1" `shouldBe` Right (LitFlon (-0.1))

  it "parse \"1/2\" => 1/2" $
    testParse rational "1/2" `shouldBe` Right (LitRatn (1 % 2))

  it "parse \"-1/10\" => -1/10" $
    testParse rational "-1/10" `shouldBe` Right (LitRatn (-1 % 10))

  it "parse \"'\\\''\" => '\\\''" $
    testParse charactor "'\''" `shouldBe` Right (LitChr '\'')

  it "parse \"'0'\" => '0'" $
    testParse charactor "'0'" `shouldBe` Right (LitChr '0')

  it "parse \"'A'\" => 'A'" $
    testParse charactor "'A'" `shouldBe` Right (LitChr 'A')

  it "parse \"'あ'\" => 'あ'" $
    testParse charactor "'あ'" `shouldBe` Right (LitChr 'あ')

  it "parse \"\\\"\\\"\" => \"\"" $
    testParse string "\"\"" `shouldBe` Right (LitStr "")

  it "parse \"\\n\" => \"\\n\"" $
    testParse string "\"\n\"" `shouldBe` Right (LitStr "\n")

  it "parse \"あ い う え お\" => \"あ い う え お\""　$
    testParse string "\"あ い う え お\"" `shouldBe`
      Right (LitStr "あ い う え お")

  it "parse \":keyword\" => :keyword" $
    testParse keyword ":keyword" `shouldBe` Right (LitKey "keyword")

  it "parse \":🍣\" => :🍣" $
    testParse keyword ":🍣" `shouldBe` Right (LitKey "🍣")
