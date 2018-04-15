{-# LANGUAGE OverloadedStrings #-}
import qualified Streaming.Base64          as Base64

import           Control.Arrow
import           Control.Monad
import           Data.ByteString.Streaming (ByteString)
import qualified Data.ByteString.Streaming as Bytes
import           Test.Tasty
import           Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ testGroup "Unit tests"
  [ encodeCase
  , decodeCase
  ]

examples =
  [ ("Man is distinguished, not only by his reason, but by this singular passion from other animals, which is a lust of the mind, that by a perseverance of delight in the continued and indefatigable generation of knowledge, exceeds the short vehemence of any carnal pleasure.",   "TWFuIGlzIGRpc3Rpbmd1aXNoZWQsIG5vdCBvbmx5IGJ5IGhpcyByZWFzb24sIGJ1dCBieSB0aGlzIHNpbmd1bGFyIHBhc3Npb24gZnJvbSBvdGhlciBhbmltYWxzLCB3aGljaCBpcyBhIGx1c3Qgb2YgdGhlIG1pbmQsIHRoYXQgYnkgYSBwZXJzZXZlcmFuY2Ugb2YgZGVsaWdodCBpbiB0aGUgY29udGludWVkIGFuZCBpbmRlZmF0aWdhYmxlIGdlbmVyYXRpb24gb2Yga25vd2xlZGdlLCBleGNlZWRzIHRoZSBzaG9ydCB2ZWhlbWVuY2Ugb2YgYW55IGNhcm5hbCBwbGVhc3VyZS4=")
  ]

encodeCase :: TestTree
encodeCase = testCase "encode" $ do
  forM_ examples $ \(input, expected) -> do
    result <- (Bytes.toLazy_ <<< Bytes.pack <<< Base64.encode <<< Bytes.fromLazy) input
    result @?= expected

decodeCase :: TestTree
decodeCase = testCase "decode" $ do
  forM_ examples $ \(expected, input) -> do
    result <- (Bytes.toLazy_ <<< Base64.decode <<< Bytes.unpack <<< Bytes.fromLazy) input
    result @?= expected
