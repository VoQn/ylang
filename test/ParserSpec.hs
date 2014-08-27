{-# OPTIONS_GHC -fno-warn-orphans #-}
module ParserSpec (spec) where

import Data.Ratio (approxRational, (%))

import Test.Hspec
import Test.QuickCheck

import Text.Parsec (parse, ParseError)
import Text.Parsec.String (Parser)
import Text.Parsec.Error (errorMessages)

import Ylang.Syntax (showRatio, Expr)
import qualified Ylang.Syntax as Y
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
    (Y.Float x, Y.Float y)
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
      Parse.bool <? "yes" `shouldParse` Y.Boolean True

    it "can parse \"no\" literal" $
      Parse.bool <? "no"  `shouldParse` Y.Boolean False

  describe "integer number literal parser" $ do

    it "can parse digit (0)" $
      Parse.int <? "0"    `shouldParse` Y.Int 0

    it "can parse hex number (0xFF)" $
      Parse.int <? "0xFF" `shouldParse` Y.Int 0xFF

    it "can parse negative integer (-10)" $
      Parse.int <? "-10"  `shouldParse` Y.Int (-10)

    it "can parse any (show (i:Integer)) string" $
      property $
        \ x -> Parse.int <? show x == Right (Y.Int x)

  describe "floating number literal parser" $ do

    let case1 = 0.0 :: Double
    it "can parse double number (0.0)" $
      Parse.float <? show case1 `shouldParse` Y.Float case1

    let case2 = 0.5 :: Double
    it "can parse double number (0.5)" $
      Parse.float <? show case2 `shouldParse` Y.Float case2

    it "can parse any (show (f:Double)) string" $
      property $
        \ x -> Parse.float <? show x ~= Right (Y.Float x)

  describe "rational number literal parser" $ do

    let case1 = 0 % 1 :: Rational
    it "can parse (0/1)" $
      Parse.ratio <? showRatio case1 `shouldParse` Y.Ratio case1

  describe "string literal parser" $ do

    it "can parse string : \"Hello\"" $
      Parse.str <? "\"Hello\"" `shouldParse` Y.String "Hello"

  describe "variable parser" $ do

    it "can parse : x" $
      Parse.variable <? "x" `shouldParse` Y.Atom "x"

  describe "operator parser" $ do

    it "can parse (+)" $
      Parse.operator <? "+" `shouldParse` Y.Atom "+"

  describe "atom parser" $ do

    it "can parse boolean : yes" $
      Parse.atom <? "yes" `shouldParse` Y.Boolean (True)

    it "can parse integer : 0" $
      Parse.atom <? "0" `shouldParse` Y.Int (0 :: Integer)

    it "can parse floating number : 0.5" $
      Parse.atom <? "0.5" `shouldParse` Y.Float (0.5 :: Double)

    it "can parse string : \"value\"" $
      Parse.atom <? "\"value\"" `shouldParse` Y.String "value"

    it "can parse operator : (*)" $
      Parse.atom <? "*" `shouldParse` Y.Atom "*"

    it "can parse varibale : `named`" $
      Parse.atom <? "named" `shouldParse` Y.Atom "named"

  describe "pair parser" $ do

    it "can parse pair (yes , no)" $
      Parse.pair <? "(yes , no)" `shouldParse`
      Y.Pair (Y.Boolean True) (Y.Boolean False)

  describe "collection parser" $ do

    it "can parse pair (1 , 2)" $
      Parse.collection <? "(1 , 2)" `shouldParse` Y.Pair (Y.Int 1) (Y.Int 2)

    it "can parse empty list : []" $
      Parse.collection <? "[]" `shouldParse` Y.Array []

  describe "arrow type parser" $ do

    it "can parse binary : (a -> a)" $
      Parse.arrow <? "(-> a a)" `shouldParse`
      Y.Arrow (Y.Atom "a") [] (Y.Atom "a")

    it "can parse nested A : (-> a (-> b a))" $
      Parse.arrow <? "(-> a (-> b a))" `shouldParse`
      Y.Arrow (Y.Atom "a") [Y.Atom "b"] (Y.Atom "a")

    it "can parse nested B : (-> (-> a b) a))" $
      Parse.arrow <? "(-> (-> a b) a)" `shouldParse`
      Y.Arrow (Y.Atom "a") [Y.Atom "b"] (Y.Atom "a")

  describe "definition parser" $ do

    it "can parse simple definition : (= x yes)" $
      Parse.define <? "(= x no)" `shouldParse`
      Y.Define (Y.Atom "x") [] (Y.Boolean False)

    it "can parse function definition : (= (id x) x)" $
      Parse.define <? "(= (id x) x)" `shouldParse`
      Y.Define (Y.Atom "id") [Y.Atom "x"] (Y.Atom "x")

    it "can parse definition (has nested expression) : (= (f x) (+ x 1))" $
      Parse.define <? "(= (f x) (+ x 1))" `shouldParse`
      Y.Define (Y.Atom "f")
               [Y.Atom "x"]
               (Y.Call (Y.Atom "+") [Y.Atom "x", Y.Int 1])

    it "can parse lambda style definition : (= seq (-> (x y) y))" $
      Parse.define <? "(= seq (-> (x y) y))" `shouldParse`
      Y.Define (Y.Atom "seq") [Y.Atom "x", Y.Atom "y"] (Y.Atom "y")

  describe "declaration parser" $ do

    it "can parse simple declaration : (: zero Int)" $
      Parse.declare <? "(: zero Int)" `shouldParse`
      Y.Declare (Y.Atom "zero") [] (Y.Atom "Int")

    it "can parse function declaration : (: (f a) a)" $
      Parse.declare <? "(: (f a) a)" `shouldParse`
      Y.Declare (Y.Atom "f") [Y.Atom "a"] (Y.Atom "a")

    it "can parse arrow type declaration : (: f (-> a a))" $
      Parse.declare <? "(: f (-> a a))" `shouldParse`
      Y.Declare (Y.Atom "f") [Y.Atom "a"] (Y.Atom "a")
