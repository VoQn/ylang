{-# OPTIONS_GHC -fno-warn-orphans #-}
module ParserSpec (spec) where

import Data.Ratio (approxRational, (%))

import Test.Hspec
import Test.QuickCheck

import Text.Parsec (parse, ParseError)
import Text.Parsec.String (Parser)
import Text.Parsec.Error (errorMessages)

import Ylang.Syntax as Syntax
import qualified Ylang.Parser as Parse

instance Eq ParseError where
   a == b = errorMessages a == errorMessages b

type ParseResult a = Either ParseError a

(<?) :: Parser a -> String -> ParseResult a
p <? t = parse p "<test>" t

(~=) :: ParseResult Expr -> ParseResult Expr -> Bool
a ~= b = case (a, b) of
  (Left  l, Left  r) -> l == r
  (Right l, Right r) -> case (l, r) of
    (Syntax.Float x, Syntax.Float y)
      -> toRatio x == toRatio y
    (x, y)
      -> x == y
  (_, _) -> False
  where
  toRatio x = approxRational x 1e-8

spec :: Spec
spec = do
  let shouldBeParse a b = a `shouldBe` Right b

  describe "boolean literal parser" $ do

    it "can parse \"yes\" literal" $
      Parse.bool <? "yes" `shouldBeParse` Syntax.Boolean True

    it "can parse \"no\" literal" $
      Parse.bool <? "no"  `shouldBeParse` Syntax.Boolean False

  describe "integer number literal parser" $ do

    it "can parse digit (0)" $
      Parse.int <? "0"    `shouldBeParse` Syntax.Int 0

    it "can parse hex number (0xFF)" $
      Parse.int <? "0xFF" `shouldBeParse` Syntax.Int 0xFF

    it "can parse negative integer (-10)" $
      Parse.int <? "-10"  `shouldBeParse` Syntax.Int (-10)

    it "can parse any (show (i:Integer)) string" $
      property $
        \ x -> Parse.int <? show x == Right (Syntax.Int x)

  describe "floating number literal parser" $ do

    let case1 = 0.0 :: Double
    it "can parse double number (0.0)" $
      Parse.float <? show case1 `shouldBeParse` Syntax.Float case1

    let case2 = 0.5 :: Double
    it "can parse double number (0.5)" $
      Parse.float <? show case2 `shouldBeParse` Syntax.Float case2

    it "can parse any (show (f:Double)) string" $
      property $
        \ x -> Parse.float <? show x ~= Right (Syntax.Float x)

  describe "rational number literal parser" $ do

    let case1 = 0 % 1 :: Rational
    it "can parse (0/1)" $
      Parse.ratio <? S.showRatio case1 `shouldBeParse` Syntax.Ratio case1

  describe "string literal parser" $ do

    it "can parse (\"Hello\")" $
      Parse.str <? "\"Hello\"" `shouldBeParse` Syntax.String "Hello"
