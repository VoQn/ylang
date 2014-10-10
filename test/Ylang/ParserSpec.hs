{-# LANGUAGE OverloadedStrings #-}
module Ylang.ParserSpec where

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
ti = FileInput { fileName = "<test>", line = 0, column = 0 }

testParse :: (Context -> Parser a) -> Text -> Either ParseError a
testParse p = parse (p ([] :: Context)) (fileName ti)

spec :: Spec
spec = describe "Ylang Syntax Parser" $ do

  describe "parse literal" $
    it "parse \"0\" => 0" $
      testParse term "0" `shouldBe`
        Right (TmLit ti{ line = 1, column = 1 } $ LitIntn 0)

  describe "parse abstruct" $ do
    it "(x:Nat) -> x" $
      testParse term "(x:Nat) -> x" `shouldBe`
        Right (TmAbs ti{ line = 1, column =  2 } "x" TyNatural $
               TmVar ti{ line = 1, column = 12 } 0 1)

    it "(x:Bool, y:Bool) -> y" $
      testParse term "(x:Bool, y:Bool) -> y" `shouldBe`
        Right (TmAbs ti{ line = 1, column =  2 } "x" TyBool $
               TmAbs ti{ line = 1, column = 10 } "y" TyBool $
               TmVar ti{ line = 1, column = 21 } 0 2)

    it "(f:Bool->Bool, x:Bool) -> f x" $
      testParse term "(f:Bool->Bool, x:Bool) -> f x" `shouldBe`
        Right (TmAbs ti{ line = 1, column =  2 } "f" (TyArrow TyBool TyBool) $
               TmAbs ti{ line = 1, column = 16 } "x" TyBool $
               TmApp ti{ line = 1, column = 27 }
                  (TmVar ti{ line = 1, column = 27} 1 2)
                  (TmVar ti{ line = 1, column = 29} 0 2))
--
