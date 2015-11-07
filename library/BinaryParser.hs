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
  limited,
)
where

import BinaryParser.Prelude
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Unsafe as ByteString
import qualified Success.Pure as Success


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
-- Run a subparser on an input from the current parser,
-- limited to the specified amount of bytes.
{-# INLINE limited #-}
limited :: Int -> BinaryParser a -> BinaryParser a
limited size (BinaryParser stateT) =
  BinaryParser $ StateT $ \remainders ->
    if ByteString.length remainders >= size
      then 
        evalStateT stateT (ByteString.unsafeTake size remainders) &
        fmap (\result -> (result, ByteString.unsafeDrop size remainders))
      else Success.failure "End of input"
