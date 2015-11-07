module BinaryParser
(
  BinaryParser,
  run,
  limited,
  total,
  failure,
  bytesOfSize,
  unitOfSize,
  unitOfBytes,
  remainders,
  endOfInput,
)
where

import BinaryParser.Prelude
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Unsafe as ByteString


-- |
-- A highly-efficient parser specialised for strict 'ByteString's.
-- 
-- Supports the roll-back and alternative branching
-- on the basis of the 'Alternative' interface.
-- 
-- Does not generate fancy error-messages,
-- which contributes to its efficiency.
newtype BinaryParser a =
  BinaryParser (forall s. STRef s ByteString -> STRef s Int -> ST s (Either Text a))

-- |
-- Apply a parser to bytes.
{-# INLINABLE run #-}
run :: BinaryParser a -> ByteString -> Either Text a
run parser bytes =
  runST (runInST parser bytes)

{-# INLINE runInST #-}
runInST :: BinaryParser a -> ByteString -> ST s (Either Text a)
runInST (BinaryParser imp) bytes =
  do
    bytesRef <- newSTRef bytes
    boundRef <- newSTRef (ByteString.length bytes)
    imp bytesRef boundRef

-- |
-- Runs a subparser on the input from the current parser,
-- limiting its range of consumption to the specified amount of bytes.
-- If the subparser tries to consume more input then it's limited to,
-- it will trigger the "Out of bounds" error.
{-# INLINABLE limited #-}
limited :: Int -> BinaryParser a -> BinaryParser a
limited newBound (BinaryParser imp) =
  BinaryParser $ \bytesRef boundRef -> do
    bound <- readSTRef boundRef
    if newBound > bound
      then
        pure $ Left $
          "New bound (" <> fromString (show newBound) <>
          ") is larger than the context bound (" <> fromString (show bound) <> ")"
      else do
        bytes <- readSTRef bytesRef
        newBoundRef <- newSTRef newBound
        result <- imp bytesRef newBoundRef
        case result of
          Left failure -> do
            writeSTRef boundRef bound
            writeSTRef bytesRef bytes
            pure (Left failure)
          Right success -> pure (Right success)

-- |
-- Fails if the parser does not consume the input in whole.
{-# INLINABLE total #-}
total :: BinaryParser a -> BinaryParser a
total (BinaryParser imp) =
  BinaryParser $ \bytesRef boundRef -> do
    boundBefore <- readSTRef boundRef
    bytesBefore <- readSTRef bytesRef
    result <- imp bytesRef boundRef
    case result of
      Left failure -> do
        writeSTRef boundRef boundBefore
        writeSTRef bytesRef bytesBefore
        pure (Left failure)
      Right success -> do
        boundAfter <- readSTRef boundRef
        if boundAfter == 0
          then pure (Right success)
          else if boundAfter > 0
            then
              pure $ Left $
                "total: " <>
                "Didn't consume the remaining " <> fromString (show boundAfter) <>
                " bytes"
            else
              error "BinaryParser.total bug: negative amount of unconsumed bytes"

-- |
-- Fails with a message.
{-# INLINE failure #-}
failure :: Text -> BinaryParser a
failure text =
  BinaryParser $ \_ _ -> pure (Left text)

-- |
-- Consumes a chunk of bytes of the specified amount.
{-# INLINABLE bytesOfSize #-}
bytesOfSize :: Int -> BinaryParser ByteString
bytesOfSize size =
  BinaryParser $ \bytesRef boundRef -> do
    bound <- readSTRef boundRef
    if size > bound
      then pure $ Left $ "bytesOfSize: Out of bounds"
      else do
        bytes <- readSTRef bytesRef
        writeSTRef bytesRef $ ByteString.unsafeDrop size bytes
        writeSTRef boundRef $ bound - size
        pure $ Right $ ByteString.unsafeTake size bytes

-- |
-- Skips an amount of bytes.
{-# INLINABLE unitOfSize #-}
unitOfSize :: Int -> BinaryParser ()
unitOfSize size =
  BinaryParser $ \bytesRef boundRef -> do
    bound <- readSTRef boundRef
    if size > bound
      then pure $ Left $ "unitOfSize: Out of bounds"
      else do
        modifySTRef bytesRef $ ByteString.unsafeDrop size
        writeSTRef boundRef $ bound - size
        pure $ Right $ ()

-- |
-- Skips specific bytes, while failing if they don't match.
{-# INLINABLE unitOfBytes #-}
unitOfBytes :: ByteString -> BinaryParser ()
unitOfBytes bytesToMatch =
  BinaryParser $ \bytesRef boundRef -> do
    bytes <- readSTRef bytesRef
    if ByteString.isPrefixOf bytesToMatch bytes
      then do
        size <- pure $ ByteString.length bytesToMatch
        writeSTRef bytesRef $ ByteString.unsafeDrop size bytes
        modifySTRef boundRef $ subtract size
        pure (Right ())
      else
        pure $ Left $ "unitOfBytes: Bytes don't match"

-- |
-- Consumes all the remaining bytes.
{-# INLINABLE remainders #-}
remainders :: BinaryParser ByteString
remainders =
  BinaryParser $ \bytesRef boundRef -> do
    bytes <- readSTRef bytesRef
    writeSTRef boundRef 0
    writeSTRef bytesRef ""
    pure (Right bytes)

-- |
-- Fails if the input hasn't ended.
{-# INLINABLE endOfInput #-}
endOfInput :: BinaryParser ()
endOfInput =
  BinaryParser $ \bytesRef boundRef -> do
    bound <- readSTRef boundRef
    if bound == 0
      then pure (Right ())
      else pure (Left "Not the end of input")

instance Functor BinaryParser where
  {-# INLINE fmap #-}
  fmap fn (BinaryParser imp) =
    BinaryParser $ \bytesRef boundRef -> fmap (fmap fn) (imp bytesRef boundRef)

instance Applicative BinaryParser where
  {-# INLINE pure #-}
  pure a =
    BinaryParser $ \_ _ -> pure (Right a)
  {-# INLINABLE (<*>) #-}
  (<*>) (BinaryParser imp1) (BinaryParser imp2) =
    BinaryParser $ \bytesRef boundRef -> do
      result1 <- imp1 bytesRef boundRef
      case result1 of
        Right success1 -> do
          result2 <- imp2 bytesRef boundRef
          case result2 of
            Right success2 -> pure (Right (success1 success2))
            Left failure2 -> pure (Left failure2)
        Left failure1 -> pure (Left failure1)

instance Alternative BinaryParser where
  {-# INLINE empty #-}
  empty =
    BinaryParser $ \_ _ -> pure (Left "")
  {-# INLINABLE (<|>) #-}
  (<|>) (BinaryParser imp1) (BinaryParser imp2) =
    BinaryParser $ \bytesRef boundRef -> do
      result1 <- imp1 bytesRef boundRef
      case result1 of
        Right success1 -> pure (Right success1)
        Left failure1 -> do
          result2 <- imp2 bytesRef boundRef
          case result2 of
            Right success2 -> pure (Right success2)
            Left failure2 -> pure (Left failure2)

instance Monad BinaryParser where
  {-# INLINE return #-}
  return =
    pure
  {-# INLINABLE (>>=) #-}
  (>>=) (BinaryParser imp1) parser2' =
    BinaryParser $ \bytesRef boundRef -> do
      result1 <- imp1 bytesRef boundRef
      case result1 of
        Right success1 -> do
          result2 <- parser2' success1 & \(BinaryParser imp2) -> imp2 bytesRef boundRef
          case result2 of
            Right success2 -> pure (Right success2)
            Left failure2 -> pure (Left failure2)
        Left failure1 -> pure (Left failure1)

instance MonadPlus BinaryParser where
  {-# INLINE mzero #-}
  mzero =
    empty
  {-# INLINE mplus #-}
  mplus =
    (<|>)
