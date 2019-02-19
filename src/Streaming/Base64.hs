-- | This module should be imported as qualified.
module Streaming.Base64 (encode, decode, Base64Exception(..)) where

-- {{{ Imports
import           Control.Arrow
import           Control.Exception.Safe
import           Control.Monad
import           Control.Monad.Trans.Class
import           Data.Bits
import           Data.ByteString.Streaming (ByteString)
import qualified Data.ByteString.Streaming as ByteString
import           Data.Char
import           Data.Function
import           Data.Maybe
import           Data.Word
import           Prelude ()
import           Prelude.Compat
import           Streaming.Prelude         (Of (..), Stream, cons, for, next,
                                            yield)
import qualified Streaming.Prelude         as Stream
-- }}}


data Base64Exception = PrematureEndOfInput deriving(Eq, Ord, Read, Show)

instance Exception Base64Exception where
  displayException PrematureEndOfInput = "Premature end of base-64 text"

characters :: String
characters = ['A'..'Z'] <> ['a'..'z'] <> ['0'..'9'] <> ['+', '/']

digits :: [Word8]
digits = [0..63]


padWord :: Word8
padWord = fromIntegral $ ord '='

data Validate a = Valid a | Invalid a

isInvalid :: Validate a -> Bool
isInvalid (Valid _) = False
isInvalid _         = True

getValid (Valid a) = Just a
getValid _         = Nothing

getValue (Valid a)   = a
getValue (Invalid a) = a

-- | Encode a binary stream in base64 format.
--
-- Output will be padded to be always a multiple of 4 bytes in length.
encode :: Monad m => ByteString m r -> Stream (Of Word8) m r
encode = ByteString.unpack >>> groupPad3 zeroBits >>> encodeStream

encodeStream :: Monad m => Stream (Of (Word8, Validate Word8, Validate Word8)) m r -> Stream (Of Word8) m r
encodeStream stream = do
  element <- lift $ next stream
  case element of
    Right ((a, b, c), stream') -> do
      let b' = getValue b
          c' = getValue c
      yield $ encodeWord $ shiftR a 2
      yield $ encodeWord $ shiftL (a .&. 0x3) 4 + shiftR b' 4
      yield $ if isInvalid b then padWord else encodeWord $ shiftL (b' .&. 0xF) 2 + shiftR c' 6
      yield $ if isInvalid c then padWord else encodeWord $ c' .&. 0x3F
      encodeStream stream'
    Left r -> return r

encodeWord :: Word8 -> Word8
encodeWord word = maybe zeroBits (fromIntegral . ord) $ lookup word $ zip digits characters

groupPad3 :: Monad m => a -> Stream (Of a) m r -> Stream (Of (a, Validate a, Validate a)) m r
groupPad3 padding stream =
  do
    element <- lift $ next stream
    case element of
      Right (a, stream2) -> do
        (b, stream3) <- nextDef padding stream2
        (c, stream4) <- nextDef padding stream3
        yield (a, b, c)
        groupPad3 padding stream4
      Left r -> return r
  where
    nextDef a s = do
      element <- lift $ next s
      return $ either (const (Invalid padding, s)) (first Valid) element


group4Pad2 :: Monad m => a -> Stream (Of a) m r -> Stream (Of (a, a, Validate a, Validate a)) m (Either Base64Exception r)
group4Pad2 padding stream = do
  element1 <- lift $ next stream
  case element1 of
    Right (a, stream2) -> do
      element2 <- lift $ next stream2
      case element2 of
        Right (b, stream3) -> do
          (c, stream4) <- nextDef padding stream3
          (d, stream5) <- nextDef padding stream4
          yield (a, b, c, d)
          group4Pad2 padding stream5
        _ -> return $ Left PrematureEndOfInput
    Left r -> return $ Right r
  where
    nextDef a s = do
      element <- lift $ next s
      return $ either (const (Invalid padding, s)) (first Valid) element


decodeStream :: Monad m => Stream (Of (Word8, Word8, Validate Word8, Validate Word8)) m r -> Stream (Of Word8) m r
decodeStream stream =
  do
    elements <- lift $ next stream
    case elements of
      Right ((a, b, c, d), stream') -> do
        let a' = decodeWord a
            b' = decodeWord b
        yield $ shiftL a' 2 + shiftR b' 4

        forM_ (fmap decodeWord $ getValid $ invalidate padWord c) $ \c' -> do
          yield $ shiftL (b' .&. 0xF) 4 + shiftR c' 2

          forM_ (fmap decodeWord $ getValid $ invalidate padWord d) $ \d' ->
            yield $ shiftL (c' .&. 0x3) 6 + d'

        decodeStream stream'
      Left r -> return r
  where
    invalidate a (Valid b) = if a == b then Invalid b else Valid b
    invalidate _ b         = b

decodeWord :: Word8 -> Word8
decodeWord word = fromMaybe zeroBits $ lookup (chr $ fromIntegral word) $ zip characters digits

-- | Decode base64-encoded data into a binary stream.
decode :: Monad m => Stream (Of Word8) m r -> ByteString m (Either Base64Exception r)
decode = group4Pad2 padWord >>> decodeStream >>> ByteString.pack
