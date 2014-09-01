{-# OPTIONS_GHC -fno-warn-orphans #-}

module ParserAtomicSpec (spec) where

import Data.Ratio

import Test.Hspec
import Test.QuickCheck

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Error

import Ylang.Syntax
import Ylang.Display (toString)
import Ylang.Parser.Atomic

instance Eq ParseError where
   a == b = errorMessages a == errorMessages b

type ParseResult a = Either ParseError a

(<?) :: Parser a -> String -> ParseResult a
p <? t = parse p "<test>" t

(~=) :: ParseResult Expr -> ParseResult Expr -> Bool
a ~= b = case (a, b) of
  (Left  l, Left  r) -> l == r
  (Right l, Right r) -> case (l, r) of
    (Float x, Float y)
      -> toRatio x == toRatio y
    (x, y)
      -> x == y
  (_, _) -> False
  where
  toRatio x = approxRational x 1e-8

shouldParse :: (Show a, Show b, Eq a, Eq b) => Either a b -> b -> Expectation
shouldParse a b = a `shouldBe` Right b

spec :: Spec
spec = do

  describe "boolean literal parser" $ do

    it "can parse \"yes\" literal" $
      bool <? "yes" `shouldParse` Boolean True

    it "can parse \"no\" literal" $
      bool <? "no"  `shouldParse` Boolean False

  describe "integer number literal parser" $ do

    it "can parse digit (0)" $
      int <? "0"    `shouldParse` Int 0

    it "can parse hex number (0xFF)" $
      int <? "0xFF" `shouldParse` Int 0xFF

    it "can parse negative integer (-10)" $
      int <? "-10"  `shouldParse` Int (-10)

    it "can parse any (show (i:Integer)) string" $
      property $
        \ x -> int <? show x == Right (Int x)

  describe "floating number literal parser" $ do

    it "can parse double number (0.0)" $ do
      let case1 = 0.0 :: Double
      float <? show case1 `shouldParse` Float case1

    it "can parse double number (0.5)" $ do
      let case2 = 0.5 :: Double
      float <? show case2 `shouldParse` Float case2

    it "can parse any (show (f:Double)) string" $
      property $
        \ x -> float <? show x ~= Right (Float x)

    it "can parse any ylang float value" $
      property $
        \ x -> float <? toString (Float x) ~= Right (Float x)

  describe "rational number literal parser" $ do

    it "can parse (0/1)" $ do
      let case1 = 0 % 1 :: Rational
      ratio <? toString (Ratio case1) `shouldParse` Ratio case1

  describe "keyword literal parser" $ do

    it "can parse keyword : :keyword" $
      keyword <? ":keyword" `shouldParse` Keyword "keyword"

  describe "charactor literal parser" $ do

    it "can parse charactor : 'c'" $
      chr <? "'c'" `shouldParse` Char 'c'

  describe "string literal parser" $ do

    it "can parse string : \"Hello\"" $
      str <? "\"Hello\"" `shouldParse` String "Hello"

  describe "variable parser" $ do

    it "can parse : x" $
      variable <? "x" `shouldParse` Atom "x"

  describe "operator parser" $ do

    it "can parse (+)" $
      operator <? "+" `shouldParse` Atom "+"

  describe "atom parser" $ do

    it "can parse boolean : yes" $
      atom <? "yes" `shouldParse` Boolean (True)

    it "can parse integer : 0" $
      atom <? "0" `shouldParse` Int (0 :: Integer)

    it "can parse floating number : 0.5" $
      atom <? "0.5" `shouldParse` Float (0.5 :: Double)

    it "can parse string : \"value\"" $
      atom <? "\"value\"" `shouldParse` String "value"

    it "can parse operator : (*)" $
      atom <? "*" `shouldParse` Atom "*"

    it "can parse varibale : `named`" $
      atom <? "named" `shouldParse` Atom "named"
