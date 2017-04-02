module Main where

import Prelude
import Test.QuickCheck.Instances
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import qualified Data.ByteString as A
import qualified Data.ByteString.Builder as C
import qualified Data.ByteString.Lazy as D
import qualified BinaryParser as B


main =
  defaultMain $
  testGroup "All tests" $
  [
    builderIsomporhismProperty "byte" B.byte C.word8
    ,
    builderIsomporhismProperty "beWord16" B.beWord16 C.word16BE
    ,
    builderIsomporhismProperty "beWord32" B.beWord32 C.word32BE
  ]

builderIsomporhismProperty subjectName parser valueToBuilder =
  testProperty name $ \value ->
  B.run parser (D.toStrict (C.toLazyByteString (valueToBuilder value))) ===
  Right value
  where
    name =
      "Builder isomorphism: " <> subjectName
