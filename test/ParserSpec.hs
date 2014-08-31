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

    it "can parse as `cons operator`" $
      pair <? "(,)" `shouldParse`
      Atom ","

    it "can parse as partial applied function (, 1)" $
      pair <? "(, 1)" `shouldParse`
      Func (Atom "x") [] [] (Pair (Int 1) (Atom "x"))

    it "can parse as partial applied function (, [])" $
      pair <? "(, [])" `shouldParse`
      Func (Atom "x") [] [] (Pair (Array []) (Atom "x"))

    it "can parse pair (, yes no)" $
      pair <? "(, yes no)" `shouldParse`
      Pair (Boolean True) (Boolean False)

    it "can parse nested list expression (, [] [])" $
      pair <? "(, [] [])" `shouldParse`
      Array [Array []]

    it "can parse primitve list (, 1 [])" $
      pair <? "(, 1 [])" `shouldParse`
      Array [Int 1]

    it "can parse another list expression (, 1 2 3 4 5 [])" $
      pair <? "(, 1 2 3 4 5 [])" `shouldParse`
      Array [Int 1,Int 2,Int 3,Int 4,Int 5]

    it "can parse dotted list expression (, 1 2 3 4 5 6)" $
      pair <? "(, 1 2 3 4 5 6)" `shouldParse`
      Pair (Array [Int 1,Int 2,Int 3,Int 4,Int 5]) (Int 6)

  describe "collection parser" $ do

    it "can parse pair (, 1 2)" $
      collection <? "(, 1 2)" `shouldParse` Pair (Int 1) (Int 2)

    it "can parse empty list : []" $
      collection <? "[]" `shouldParse` Array []

  describe "arrow type parser" $ do

    it "can parse binary : (-> a a)" $
      arrow <? "(-> a a)" `shouldParse`
      Arrow (Atom "a") [] (Atom "a")

    it "can parse multiple : (-> a b a)" $
      arrow <? "(-> a b a)" `shouldParse`
      Arrow (Atom "a") [Atom "b"] (Atom "a")

    it "can parse nested A : (-> a (-> b a))" $
      arrow <? "(-> a (-> b a))" `shouldParse`
      Arrow (Atom "a") [] (Arrow (Atom "b") [] (Atom "a"))

    it "can parse nested B : (-> (-> a b) a))" $
      arrow <? "(-> (-> a b) a)" `shouldParse`
      Arrow (Arrow (Atom "a") [] (Atom "b")) [] (Atom "a")

  describe "closure parser" $ do

    it "can parse simple function (\\ x x)" $
      closure <? "(\\ x x)" `shouldParse`
      Func (Atom "x") [] [] (Atom "x")

    it "can parse multiple argument function (\\ (x y) y)" $
      closure <? "(\\ (x y) y)" `shouldParse`
      Func (Atom "x") [Atom "y"] [] (Atom "y")

    it "can parse (\\ x y z)" $
      closure <? "(\\ x y z)" `shouldParse`
      Func (Atom "x") [] [Atom "y"] (Atom "z")

    it "can parse (\\ x (y) z))" $
      closure <? "(\\ x (y) z))" `shouldParse`
      Func (Atom "x") [] [Atom "y"] (Atom "z")

    it "can parse (\\ x (y z))" $
      closure <? "(\\ x (y z))" `shouldParse`
      Func (Atom "x") [] [] (Factor [Atom "y", Atom "z"])

  describe "definition parser" $ do

    it "can parse simple definition : (= x yes)" $
      define <? "(= x no)" `shouldParse`
      Define "x" (Boolean False)

    it "can parse function definition : (= (id x) x)" $
      define <? "(= (id x) x)" `shouldParse`
      Define "id" (Func (Atom "x") [] [] (Atom "x"))

    it "can parse definition (has nested expression) : (= (f x) (+ x 1))" $
      define <? "(= (f x) (+ x 1))" `shouldParse`
      Define "f" (Func (Atom "x") [] [] (Factor [Atom "+",Atom "x",Int 1]))

    it "can parse lambda style definition : (= seq (\\ (x y) y))" $
      define <? "(= seq (\\ (x y) y))" `shouldParse`
      Define "seq" (Func (Atom "x") [Atom "y"] [] (Atom "y"))

  describe "declaration parser" $ do

    it "can parse simple declaration : (: zero Int)" $
      declare <? "(: zero Int)" `shouldParse`
      Declare "zero" [] [] (Atom "Int")

    it "can parse pair type : (: age-name (, Int String))" $
      declare <? "(: age-name (, Int String))" `shouldParse`
      Declare "age-name" [] [] (Pair (Atom "Int") (Atom "String"))

    it "can parse pair type : (: events [String])" $
      declare <? "(: events [String]))" `shouldParse`
      Declare "events" [] [] (Array [Atom "String"])

    it "can parse variable type declaration : (: zero (: a Addible) a)" $
      declare <? "(: zero (: a Addible) a)" `shouldParse`
      Declare "zero" [Declare "a" [] [] (Atom "Addible")] [] (Atom "a")

    it "can parse arrow type declaration : (: f (-> a a))" $
      declare <? "(: f (-> a a))" `shouldParse`
      Declare "f" [] [Atom "a"] (Atom "a")

    it "can parse comprex declaration : (: f (: t Set) (-> t t))" $
      declare <? "(: f (: t Set) (-> t t))" `shouldParse`
      Declare "f" [Declare "t" [] [] (Atom "Set")] [Atom "t"] (Atom "t")
