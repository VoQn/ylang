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
      Parse.ratio <? Syntax.showRatio case1 `shouldBeParse` Syntax.Ratio case1

  describe "string literal parser" $ do

    it "can parse string : \"Hello\"" $
      Parse.str <? "\"Hello\"" `shouldBeParse` Syntax.String "Hello"

  describe "variable parser" $ do

    it "can parse : x" $
      Parse.variable <? "x" `shouldBeParse` Syntax.Var "x"

  describe "operator parser" $ do

    it "can parse (+)" $
      Parse.operator <? "+" `shouldBeParse` Syntax.Operator "+"

  describe "atom parser" $ do

    it "can parse boolean : yes" $
      Parse.atom <? "yes" `shouldBeParse` Syntax.Boolean (True)

    it "can parse integer : 0" $
      Parse.atom <? "0" `shouldBeParse` Syntax.Int (0 :: Integer)

    it "can parse floating number : 0.5" $
      Parse.atom <? "0.5" `shouldBeParse` Syntax.Float (0.5 :: Double)

    it "can parse string : \"value\"" $
      Parse.atom <? "\"value\"" `shouldBeParse` Syntax.String "value"

    it "can parse operator : (*)" $
      Parse.atom <? "*" `shouldBeParse` Syntax.Operator "*"

    it "can parse varibale : `named`" $
      Parse.atom <? "named" `shouldBeParse` Syntax.Var "named"

  describe "collection parser" $ do

    it "can parse empty list : []" $
      Parse.collection <? "[]" `shouldBeParse` Syntax.List []

  describe "definition parser" $ do

    it "can parse simple definition : (= x yes)" $
      Parse.define <? "(= x no)" `shouldBeParse`
      Syntax.Define (Syntax.Var "x") [] (Syntax.Boolean False)

    it "can parse function definition : (= (id x) x)" $
      Parse.define <? "(= (id x) x)" `shouldBeParse`
      Syntax.Define (Syntax.Var "id") [Syntax.Var "x"] (Syntax.Var "x")

    it "can parse definition (has nested expression) : (= (f x) (+ x 1))" $
      Parse.define <? "(= (f x) (+ x 1))" `shouldBeParse`
      Syntax.Define (Syntax.Var "f")
                    [Syntax.Var "x"]
                    (Syntax.Call (Syntax.Operator "+")
                                 [Syntax.Var "x", Syntax.Int 1])

    it "can parse lambda style definition : (= seq ((\\ x y) y))" $
      Parse.define <? "(= seq ((\\ x y) y))" `shouldBeParse`
      Syntax.Define (Syntax.Var "seq")
                    [Syntax.Var "x", Syntax.Var "y"]
                    (Var "y")

  describe "declaration parser" $ do

    it "can parse simple declaration : (: zero Int)" $
      Parse.declare <? "(: zero Int)" `shouldBeParse`
      Syntax.Declare (Var "zero") [] (Var "Int")

    it "can parse function declaration : (: (f a) a)" $
      Parse.declare <? "(: (f a) a)" `shouldBeParse`
      Syntax.Declare (Var "f") [Var "a"] (Var "a")

    it "can parse arrow type declaration : (: f (-> a a))" $
      Parse.declare <? "(: f (-> a a))" `shouldBeParse`
      Syntax.Declare (Var "f") [Var "a"] (Var "a")
