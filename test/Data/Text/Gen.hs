{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Text.Gen where

import Control.Applicative
import Data.Text (Text)
import Data.Text.Encoding
import Data.Word
import Test.QuickCheck

import qualified Data.ByteString as B

instance Arbitrary Text where
  arbitrary = decodeUtf8 . B.pack <$> listOf1 genWord

genWord :: Gen Word8
genWord = frequency [
    (  1, genSymWord)
  , ( 10, genNumWord)
  , (100, genAlphaWord)
  ]

genSymWord :: Gen Word8
genSymWord = oneof [
    pure 0x0020
  , elements [0x0023..0x0026]
  , elements [0x003C..0x0040]
  ]

genNumWord :: Gen Word8
genNumWord = elements [0x0030..0x0039]

genAlphaWord :: Gen Word8
genAlphaWord = elements [0x0041..0x007A]
