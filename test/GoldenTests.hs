{-# LANGUAGE OverloadedStrings #-}
import qualified Streaming.Base64          as Base64

import           Control.Arrow
import           Control.Monad
import           Data.ByteString.Streaming (ByteString)
import qualified Data.ByteString.Streaming as Bytes
import           Streaming.With
import           System.FilePath
import           Test.Tasty
import           Test.Tasty.Golden         (findByExtension, goldenVsString)

main :: IO ()
main = defaultMain =<< do
  decodedFiles <- findByExtension [".decoded"] "."
  encodedFiles <- findByExtension [".encoded"] "."

  return $ testGroup "Golden tests" $ map encodingTest decodedFiles <> map decodingTest encodedFiles

encodingTest :: FilePath -> TestTree
encodingTest decodedFile = goldenVsString name encodedFile f where
  encodedFile = replaceExtension decodedFile "encoded"
  name = "Encoding " <> dropExtension decodedFile
  f = withBinaryFileContents decodedFile (Bytes.toLazy_ <<< Bytes.pack <<< Base64.encode)

decodingTest :: FilePath -> TestTree
decodingTest encodedFile = goldenVsString name decodedFile f where
  decodedFile = replaceExtension encodedFile "decoded"
  name = "Decoding " <> dropExtension encodedFile
  f = withBinaryFileContents encodedFile (Bytes.toLazy_ <<< Base64.decode <<< Bytes.unpack)
