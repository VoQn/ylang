{-# OPTIONS_GHC -fno-warn-orphans #-}
module ParserSpec (spec) where

import Data.Ratio (approxRational)

import Test.Hspec
import Test.QuickCheck

import Text.Parsec (parse, ParseError)
import Text.Parsec.String (Parser)
import Text.Parsec.Error (errorMessages)

import Ylang.Syntax as S
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
    (S.Float x, S.Float y) -> toRatio x == toRatio y
    (x, y) -> x == y
  (_, _) -> False
  where
  toRatio x = x `approxRational` 1e-8

spec :: Spec
spec = do

  describe "boolean literal parser" $ do

    it "can parse \"yes\" literal" $
      Parse.bool <? "yes" `shouldBe` (Right $ S.Boolean True)

    it "can parse \"no\" literal" $
      Parse.bool <? "no" `shouldBe` (Right $ S.Boolean False)

  describe "interger number literal parser" $ do

    it "can parse digit (0)" $
      Parse.int <? "0" `shouldBe` (Right $ S.Int 0)

    it "can parse hex number (0xFF)" $
      Parse.int <? "0xFF" `shouldBe` (Right $ S.Int 0xFF)

    it "can parse negative integer (-10)" $
      Parse.int <? "-10" `shouldBe` (Right $ S.Int (-10))

    it "can parse any (show (i:Integer)) string" $
      property $
        \ x -> Parse.int <? show x == (Right $ S.Int x)

  describe "floating number literal parser" $ do

    let case1 = 0.0 :: Double
    it "can parse double number (0.0)" $
      Parse.float <? (show case1) `shouldBe` (Right $ S.Float case1)

    let case2 = 0.5 :: Double
    it "can parse double number (0.5)" $
      Parse.float <? (show case2) `shouldBe` (Right $ S.Float case2)

    it "can parse any (show (f:Double)) string" $
      property $
        \ x -> Parse.float <? show x ~= (Right $ S.Float x)

  describe "string literal parser" $ do

    it "can parse (\"Hello\")" $
      Parse.str <? "\"Hello\"" `shouldBe` (Right $ S.String "Hello")
