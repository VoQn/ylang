{-# LANGUAGE OverloadedStrings #-}
module Ylang.ParserSpec where

import Data.Ratio
import Data.Text  (Text)
import Test.Hspec
import Text.Parsec
import Text.Parsec.Text (Parser)
import Ylang.Info
import Ylang.Context
import Ylang.Type
import Ylang.Syntax.Literal
import Ylang.Syntax.Term
import Ylang.Parser
import Ylang.Parser.TokenSpec()

ti :: Info
ti = FileInput { fileName = "<test>", line = 1, column = 1 }

testParse :: (Context -> Parser a) -> Text -> Either ParseError a
testParse p = p empty `parse` fileName ti
  where
  empty = []

spec :: Spec
spec = describe "Ylang Syntax Parser" $ do

  describe "parse literal" $ do

    it "0" $
      testParse term "0" `shouldBe`
        Right (TmLit ti $ LitIntn 0)

    it "1/2" $
      testParse term "1/2" `shouldBe`
        Right (TmLit ti $ LitRatn $ 1 % 2)

    it "Yes" $
      testParse term "Yes" `shouldBe`
        Right (TmLit ti $ LitBool True)

  describe "parse abstruct" $ do

    it "(x:Set) -> x" $
      testParse term "(x:Set) -> x" `shouldBe`
        Right (TmAbs ti{ column =  2 } "x" TyTop $
               TmVar ti{ column = 12 } 0 1)

    it "(x:Set, y:Set) -> y" $
      testParse term "(x:Set, y:Set) -> y" `shouldBe`
        Right (TmAbs ti{ column =  2 } "x" TyTop $
               TmAbs ti{ column =  9 } "y" TyTop $
               TmVar ti{ column = 19 } 0 2)

    it "(f:Bool->Nat, x:Bool) -> f x" $
      testParse term "(f:Bool->Nat, x:Bool) -> f x" `shouldBe`
        Right (TmAbs   ti{ column =  2 } "f" (TyArrow TyBool TyNatural) $
               TmAbs   ti{ column = 15 } "x" TyBool $
               TmApp   ti{ column = 26 }
                (TmVar ti{ column = 26 } 1 2)
                (TmVar ti{ column = 28 } 0 2))

  describe "parse apply" $ do

    it "((x:Integer) -> x) 10" $
      testParse term "((x:Integer) -> x) 10" `shouldBe`
        Right (TmApp   ti{ column =  1 }
                (TmAbs ti{ column =  3 } "x" TyInteger $
                 TmVar ti{ column = 17 } 0 1)
                (TmLit ti{ column = 20 } $ LitIntn 10))

    it "((a:Bool, b:Bool) -> b) Yes No" $
      testParse term "((a:Bool, b:Bool) -> a) Yes No" `shouldBe`
        Right (TmApp     ti{ column =  1 }
                (TmApp   ti{ column =  1 }
                  (TmAbs ti{ column =  3 } "a" TyBool $
                   TmAbs ti{ column = 11 } "b" TyBool $
                   TmVar ti{ column = 22 } 1 2)
                  (TmLit ti{ column = 25 } $ LitBool True))
                (TmLit   ti{ column = 29 } $ LitBool False))
--
