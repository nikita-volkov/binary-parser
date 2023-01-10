module Main where

import BasePrelude
import qualified BinaryParser as B
import qualified Data.ByteString as A
import qualified Data.ByteString.Builder as C
import qualified Data.ByteString.Lazy as D
import Test.QuickCheck.Instances
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

main =
  defaultMain $
    testGroup "All tests" $
      [ builderIsomporhismProperty "byte" B.byte C.word8,
        builderIsomporhismProperty "beWord16" B.beWord16 C.word16BE,
        builderIsomporhismProperty "beWord32" B.beWord32 C.word32BE,
        expectedResultTest "byte consumes" ((,) <$> B.byte <*> B.beWord32) (49, 4) "1\NUL\NUL\NUL\EOT",
        expectedResultTest "Applicative composition" ((,) <$> B.beWord16 <*> B.beWord16) (1, 2) "\NUL\SOH\NUL\STX",
        let parser =
              do
                a <- B.beWord16
                b <- B.beWord16
                return (a, b)
         in expectedResultTest "Monadic composition" parser (1, 2) "\NUL\SOH\NUL\STX"
      ]

builderIsomporhismProperty details parser valueToBuilder =
  testProperty name $ \value ->
    B.run parser (D.toStrict (C.toLazyByteString (valueToBuilder value)))
      === Right value
  where
    name =
      "builderIsomporhismProperty: " <> details

expectedResultTest details parser expectedValue input =
  testCase name $ do
    assertEqual "" (Right expectedValue) (B.run parser input)
  where
    name =
      "expectedResultTest: " <> details
