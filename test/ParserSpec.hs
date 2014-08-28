{-# OPTIONS_GHC -fno-warn-orphans #-}
module ParserSpec (spec) where

import Test.Hspec

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Error

import Ylang.Syntax
import Ylang.Parser

instance Eq ParseError where
   a == b = errorMessages a == errorMessages b

type ParseResult a = Either ParseError a

(<?) :: Parser a -> String -> ParseResult a
p <? t = parse p "<test>" t

shouldParse :: (Show a, Show b, Eq a, Eq b) => Either a b -> b -> Expectation
shouldParse a b = a `shouldBe` Right b

spec :: Spec
spec = do
  describe "pair parser" $ do

    it "can parse pair (, yes no)" $
      pair <? "(, yes no)" `shouldParse`
      Pair (Boolean True) (Boolean False)

  describe "collection parser" $ do

    it "can parse pair (, 1 2)" $
      collection <? "(, 1 2)" `shouldParse` Pair (Int 1) (Int 2)

    it "can parse empty list : []" $
      collection <? "[]" `shouldParse` Array []

  describe "arrow type parser" $ do

    it "can parse binary : (a -> a)" $
      arrow <? "(-> a a)" `shouldParse`
      Factor [Atom "->",Atom "a",Atom "a"]

    it "can parse nested A : (-> a (-> b a))" $
      arrow <? "(-> a (-> b a))" `shouldParse`
      Factor [Atom "->",Atom "a",(Factor [Atom "->", Atom "b",Atom "a"])]

    it "can parse nested B : (-> (-> a b) a))" $
      arrow <? "(-> (-> a b) a)" `shouldParse`
      Factor [Atom "->",Factor[Atom "->",Atom "a",Atom "b"],Atom "a"]

  describe "definition parser" $ do

    it "can parse simple definition : (= x yes)" $
      define <? "(= x no)" `shouldParse`
      Factor [Atom "=",Atom "x",Boolean False]

    it "can parse function definition : (= (id x) x)" $
      define <? "(= (id x) x)" `shouldParse`
      Factor [Atom "=",Factor [Atom "id", Atom "x"],Atom "x"]

    it "can parse definition (has nested expression) : (= (f x) (+ x 1))" $
      define <? "(= (f x) (+ x 1))" `shouldParse`
      Factor [Atom "=",Factor [Atom "f",Atom "x"],Factor [Atom "+",Atom "x",Int 1]]

    it "can parse lambda style definition : (= seq (\\ (x y) y))" $
      define <? "(= seq (\\ (x y) y))" `shouldParse`
      Factor [Atom "=",Atom "seq",Factor [Atom "\\",Factor[Atom "x",Atom "y"] ,Atom "y"]]

  describe "declaration parser" $ do

    it "can parse simple declaration : (: zero Int)" $
      declare <? "(: zero Int)" `shouldParse`
      Factor [Atom ":",Atom "zero",Atom "Int"]

    it "can parse function declaration : (: (f a) a)" $
      declare <? "(: (f a) a)" `shouldParse`
      Factor [Atom ":",Factor[Atom "f",Atom "a"],Atom "a"]

    it "can parse arrow type declaration : (: f (-> a a))" $
      declare <? "(: f (-> a a))" `shouldParse`
      Factor [Atom ":",Atom "f",Factor [Atom "->",Atom "a",Atom "a"]]
