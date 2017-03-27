{-# LANGUAGE CPP #-}
module BinaryParser
(
  BinaryParser,
  run,
  failure,
  byte,
  bytesOfSize,
  unitOfSize,
  unitOfBytes,
  remainders,
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
)
where

import BinaryParser.Prelude
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Unsafe as ByteString
import qualified Success.Pure as Success
import qualified Data.ByteString.Internal as A


-- |
-- A highly-efficient parser specialised for strict 'ByteString's.
-- 
-- Supports the roll-back and alternative branching
-- on the basis of the 'Alternative' interface.
-- 
-- Does not generate fancy error-messages,
-- which contributes to its efficiency.
newtype BinaryParser a =
  BinaryParser ( StateT ByteString ( Success.Success Text ) a )
  deriving ( Functor , Applicative , Alternative , Monad , MonadPlus )

-- |
-- Apply a parser to bytes.
{-# INLINE run #-}
run :: BinaryParser a -> ByteString -> Either Text a
run (BinaryParser parser) input =
  mapLeft fold (Success.asEither (evalStateT parser input))

-- |
-- Fail with a message.
{-# INLINE failure #-}
failure :: Text -> BinaryParser a
failure text =
  BinaryParser (lift (Success.failure text))

-- |
-- Consume a single byte.
{-# INLINE byte #-}
byte :: BinaryParser Word8
byte =
  BinaryParser $ StateT $ \remainders ->
    if ByteString.null remainders
      then Success.failure "End of input"
      else pure (ByteString.unsafeHead remainders, ByteString.unsafeDrop 1 remainders)

-- |
-- Consume an amount of bytes.
{-# INLINE bytesOfSize #-}
bytesOfSize :: Int -> BinaryParser ByteString
bytesOfSize size =
  BinaryParser $ StateT $ \remainders ->
    if ByteString.length remainders >= size
      then return (ByteString.unsafeTake size remainders, ByteString.unsafeDrop size remainders)
      else Success.failure "End of input"

-- |
-- Skip an amount of bytes.
{-# INLINE unitOfSize #-}
unitOfSize :: Int -> BinaryParser ()
unitOfSize size =
  BinaryParser $ StateT $ \remainders ->
    if ByteString.length remainders >= size
      then return ((), ByteString.unsafeDrop size remainders)
      else Success.failure "End of input"

-- |
-- Skip specific bytes, while failing if they don't match.
{-# INLINE unitOfBytes #-}
unitOfBytes :: ByteString -> BinaryParser ()
unitOfBytes bytes =
  BinaryParser $ StateT $ \remainders ->
    if ByteString.isPrefixOf bytes remainders
      then return ((), ByteString.unsafeDrop (ByteString.length bytes) remainders)
      else Success.failure "Bytes don't match"

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
    _ -> Success.failure "Not the end of input"

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
      else Success.failure "End of input"

-- |
-- Storable value of the given amount of bytes.
{-# INLINE storableOfSize #-}
storableOfSize :: Storable a => Int -> BinaryParser a
storableOfSize size =
  BinaryParser $ StateT $ \(A.PS payloadFP offset length) ->
    if length >= size
      then let result = unsafeDupablePerformIO $ withForeignPtr payloadFP $ \ptr -> peek (castPtr ptr)
               newRemainder = A.PS payloadFP (offset + size) (length - size)
               in return (result, newRemainder)
      else Success.failure "End of input" 

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
