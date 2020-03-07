{-# LANGUAGE CPP #-}
module BinaryParser
(
  BinaryParser,
  run,
  failure,
  byte,
  matchingByte,
  bytesOfSize,
  bytesWhile,
  unitOfSize,
  unitOfBytes,
  unitWhile,
  remainders,
  fold,
  endOfInput,
  sized,
  -- * Extras
  storableOfSize,
  beWord16,
  leWord16,
  beWord32,
  leWord32,
  beWord64,
  leWord64,
  asciiIntegral,
)
where

import BinaryParser.Prelude hiding (fold)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Unsafe as ByteString
import qualified Data.ByteString.Internal as A
import qualified BinaryParser.Prelude as B
import BinaryParser.Internal(BinaryParser(..))

-- |
-- Apply a parser to bytes.
{-# INLINE run #-}
run :: BinaryParser a -> ByteString -> Either Text a
run (BinaryParser parser) input =
  runExcept (evalStateT parser input)

-- |
-- Fail with a message.
{-# INLINE failure #-}
failure :: Text -> BinaryParser a
failure text =
  BinaryParser (lift (throwE text))

-- |
-- Consume a single byte.
{-# INLINE byte #-}
byte :: BinaryParser Word8
byte =
  BinaryParser $ StateT $ \remainders ->
    if ByteString.null remainders
      then throwE "End of input"
      else pure (ByteString.unsafeHead remainders, ByteString.unsafeDrop 1 remainders)

-- |
-- Consume a single byte, which satisfies the predicate.
{-# INLINE satisfyingByte #-}
satisfyingByte :: (Word8 -> Bool) -> BinaryParser Word8
satisfyingByte predicate =
  BinaryParser $ StateT $ \remainders ->
    case ByteString.uncons remainders of
      Nothing -> throwE "End of input"
      Just (head, tail) ->
        if predicate head
          then pure (head, tail)
          else throwE "Byte doesn't satisfy a predicate"

-- |
-- Consume a single byte, which satisfies the predicate.
{-# INLINE matchingByte #-}
matchingByte :: (Word8 -> Either Text a) -> BinaryParser a
matchingByte matcher =
  BinaryParser $ StateT $ \remainders ->
    case ByteString.uncons remainders of
      Nothing -> throwE "End of input"
      Just (head, tail) ->
        case matcher head of
          Right result -> pure (result, tail)
          Left error -> throwE error

-- |
-- Consume an amount of bytes.
{-# INLINE bytesOfSize #-}
bytesOfSize :: Int -> BinaryParser ByteString
bytesOfSize size =
  BinaryParser $ StateT $ \remainders ->
    if ByteString.length remainders >= size
      then return (ByteString.unsafeTake size remainders, ByteString.unsafeDrop size remainders)
      else throwE "End of input"

-- |
-- Consume multiple bytes, which satisfy the predicate.
{-# INLINE bytesWhile #-}
bytesWhile :: (Word8 -> Bool) -> BinaryParser ByteString
bytesWhile predicate =
  BinaryParser $ StateT $ \remainders ->
    pure (ByteString.span predicate remainders)

-- |
-- Skip an amount of bytes.
{-# INLINE unitOfSize #-}
unitOfSize :: Int -> BinaryParser ()
unitOfSize size =
  BinaryParser $ StateT $ \remainders ->
    if ByteString.length remainders >= size
      then return ((), ByteString.unsafeDrop size remainders)
      else throwE "End of input"

-- |
-- Skip specific bytes, while failing if they don't match.
{-# INLINE unitOfBytes #-}
unitOfBytes :: ByteString -> BinaryParser ()
unitOfBytes bytes =
  BinaryParser $ StateT $ \remainders ->
    if ByteString.isPrefixOf bytes remainders
      then return ((), ByteString.unsafeDrop (ByteString.length bytes) remainders)
      else throwE "Bytes don't match"

-- |
-- Skip bytes, which satisfy the predicate.
{-# INLINE unitWhile #-}
unitWhile :: (Word8 -> Bool) -> BinaryParser ()
unitWhile predicate =
  BinaryParser $ StateT $ \remainders ->
    pure ((), ByteString.dropWhile predicate remainders)

-- |
-- Consume all the remaining bytes.
{-# INLINE remainders #-}
remainders :: BinaryParser ByteString
remainders =
  BinaryParser $ StateT $ \remainders -> return (remainders, ByteString.empty)

-- |
-- Fail if the input hasn't ended.
{-# INLINE endOfInput #-}
endOfInput :: BinaryParser ()
endOfInput =
  BinaryParser $ StateT $ \case
    "" -> return ((), ByteString.empty)
    _ -> throwE "Not the end of input"

-- |
-- Left-fold the bytes, terminating before the byte,
-- on which the step function returns Nothing.
{-# INLINE fold #-}
fold :: (a -> Word8 -> Maybe a) -> a -> BinaryParser a
fold step init =
  BinaryParser $ StateT $ return . loop init
  where
    loop !accumulator remainders =
      case ByteString.uncons remainders of
        Nothing -> (accumulator, remainders)
        Just (head, tail) ->
          case step accumulator head of
            Just newAccumulator ->
              loop newAccumulator tail
            Nothing -> (accumulator, remainders)

-- |
-- Run a subparser passing it a chunk of the current input of the specified size.
{-# INLINE sized #-}
sized :: Int -> BinaryParser a -> BinaryParser a
sized size (BinaryParser stateT) =
  BinaryParser $ StateT $ \remainders ->
    if ByteString.length remainders >= size
      then 
        evalStateT stateT (ByteString.unsafeTake size remainders) &
        fmap (\result -> (result, ByteString.unsafeDrop size remainders))
      else throwE "End of input"

-- |
-- Storable value of the given amount of bytes.
{-# INLINE storableOfSize #-}
storableOfSize :: Storable a => Int -> BinaryParser a
storableOfSize size =
  BinaryParser $ StateT $ \(A.PS payloadFP offset length) ->
    if length >= size
      then let result =
                 unsafeDupablePerformIO $ withForeignPtr payloadFP $ \ptr -> peekByteOff (castPtr ptr) offset
               newRemainder =
                 A.PS payloadFP (offset + size) (length - size)
               in return (result, newRemainder)
      else throwE "End of input" 

-- | Big-endian word of 2 bytes.
{-# INLINE beWord16 #-}
beWord16 :: BinaryParser Word16
#ifdef WORDS_BIGENDIAN
beWord16 =
  storableOfSize 2
#else
beWord16 =
  byteSwap16 <$> storableOfSize 2
#endif

-- | Little-endian word of 2 bytes.
{-# INLINE leWord16 #-}
leWord16 :: BinaryParser Word16
#ifdef WORDS_BIGENDIAN
leWord16 =
  byteSwap16 <$> storableOfSize 2
#else
leWord16 =
  storableOfSize 2
#endif

-- | Big-endian word of 4 bytes.
{-# INLINE beWord32 #-}
beWord32 :: BinaryParser Word32
#ifdef WORDS_BIGENDIAN
beWord32 =
  storableOfSize 4
#else
beWord32 =
  byteSwap32 <$> storableOfSize 4
#endif

-- | Little-endian word of 4 bytes.
{-# INLINE leWord32 #-}
leWord32 :: BinaryParser Word32
#ifdef WORDS_BIGENDIAN
leWord32 =
  byteSwap32 <$> storableOfSize 4
#else
leWord32 =
  storableOfSize 4
#endif

-- | Big-endian word of 8 bytes.
{-# INLINE beWord64 #-}
beWord64 :: BinaryParser Word64
#ifdef WORDS_BIGENDIAN
beWord64 =
  storableOfSize 8
#else
beWord64 =
  byteSwap64 <$> storableOfSize 8
#endif

-- | Little-endian word of 8 bytes.
{-# INLINE leWord64 #-}
leWord64 :: BinaryParser Word64
#ifdef WORDS_BIGENDIAN
leWord64 =
  byteSwap64 <$> storableOfSize 8
#else
leWord64 =
  storableOfSize 8
#endif

-- |
-- Integral number encoded in ASCII.
{-# INLINE asciiIntegral #-}
asciiIntegral :: Integral a => BinaryParser a
asciiIntegral = 
  do
    firstDigit <- matchingByte byteDigit
    fold step firstDigit
  where
    byteDigit byte =
      case byte - 48 of
        subtracted ->
          if subtracted <= 9
            then Right (fromIntegral subtracted)
            else Left "Not an ASCII decimal byte"
    step state byte =
      case byteDigit byte of
        Right digit -> Just (state * 10 + digit)
        _ -> Nothing
