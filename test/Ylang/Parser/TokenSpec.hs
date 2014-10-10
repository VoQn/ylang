{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Ylang.Parser.TokenSpec where

import Test.Hspec
import Test.Hspec.QuickCheck

import Data.Ratio        ((%))
import Data.Text         (Text)
import Text.Parsec       (ParseError, parse)
import Text.Parsec.Text  (Parser)
import Text.Parsec.Error (errorMessages)

import Ylang.IO
import Ylang.Syntax.Literal (Lit(..))
import Ylang.Syntax.LiteralSpec()
import Ylang.Parser.Token

import qualified Data.Text.Lazy         as L
import qualified Data.Text.Lazy.Builder as LB

instance Eq ParseError where
  a == b = errorMessages a == errorMessages b

testParse :: Parser a -> Text -> Either ParseError a
testParse p = parse p "<test>"

spec :: Spec
spec = describe "Ylang Lexer" $ do

  it "parse \"_\" => _" $
    testParse literal "_" `shouldBe` Right LitHole

  it "parse \"()\" => ()" $
    testParse literal "()" `shouldBe` Right LitUnit

  it "parse \"Yes\" => yes" $
    testParse literal "Yes" `shouldBe` Right (LitBool True)

  it "parse \"No\" => no" $
    testParse literal "No" `shouldBe` Right (LitBool False)

  it "parse \"0\" => 0" $
    testParse literal "0" `shouldBe` Right (LitIntn 0)

  it "parse \"-10\" => -10" $
    testParse literal "-10" `shouldBe` Right (LitIntn (-10))

  it "parse \"0.1\" => 0.1" $
    testParse literal "0.1" `shouldBe` Right (LitFlon 0.1)

  it "parse \"-0.1\" => -0.1" $
    testParse literal "-0.1" `shouldBe` Right (LitFlon (-0.1))

  it "parse \"1/2\" => 1/2" $
    testParse literal "1/2" `shouldBe` Right (LitRatn (1 % 2))

  it "parse \"-1/10\" => -1/10" $
    testParse literal "-1/10" `shouldBe` Right (LitRatn (-1 % 10))

  it "parse \"'\\\''\" => '\\\''" $
    testParse literal "'\\''" `shouldBe` Right (LitChr '\'')

  it "parse \"'0'\" => '0'" $
    testParse literal "'0'" `shouldBe` Right (LitChr '0')

  it "parse \"'A'\" => 'A'" $
    testParse literal "'A'" `shouldBe` Right (LitChr 'A')

  it "parse \"'あ'\" => 'あ'" $
    testParse literal "'あ'" `shouldBe` Right (LitChr 'あ')

  it "parse \"\\\"\\\"\" => \"\"" $
    testParse literal "\"\"" `shouldBe` Right (LitStr "")

  it "parse \"\\n\" => \"\\n\"" $
    testParse literal "\"\\n\"" `shouldBe` Right (LitStr "\n")

  it "parse \"あ い う え お\" => \"あ い う え お\""　$
    testParse literal "\"あ い う え お\"" `shouldBe`
      Right (LitStr "あ い う え お")

  it "parse \"🍣 🐟 🍚 👍\" => \"🍣 🐟 🍚 👍\""　$
    testParse literal "\"🍣 🐟 🍚 👍\"" `shouldBe`
      Right (LitStr "🍣 🐟 🍚 👍")

  it "parse \":keyword\" => :keyword" $
    testParse literal ":keyword" `shouldBe` Right (LitKey "keyword")

  it "parse \":🍣\" => :🍣" $
    testParse literal ":🍣" `shouldBe` Right (LitKey "🍣")
