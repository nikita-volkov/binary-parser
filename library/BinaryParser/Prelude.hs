module BinaryParser.Prelude
( 
  module Exports,
  LazyByteString,
  ByteStringBuilder,
  LazyText,
  TextBuilder,
  mapLeft,
  joinMap,
)
where


-- base-prelude
-------------------------
import BasePrelude as Exports hiding (fail)

-- transformers
-------------------------
import Control.Monad.Trans.State.Strict as Exports hiding (liftCallCC, liftCatch)
import Control.Monad.Trans.Reader as Exports hiding (liftCallCC, liftCatch)
import Control.Monad.Trans.Class as Exports

-- bytestring
-------------------------
import Data.ByteString as Exports (ByteString)

-- text
-------------------------
import Data.Text as Exports (Text)

-- custom
-------------------------
import qualified Data.ByteString.Lazy
import qualified Data.ByteString.Builder
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Builder


type LazyByteString =
  Data.ByteString.Lazy.ByteString

type ByteStringBuilder =
  Data.ByteString.Builder.Builder

type LazyText =
  Data.Text.Lazy.Text

type TextBuilder =
  Data.Text.Lazy.Builder.Builder


{-# INLINE mapLeft #-}
mapLeft :: (a -> b) -> Either a x -> Either b x
mapLeft f =
  either (Left . f) Right

joinMap :: Monad m => (a -> m b) -> m a -> m b
joinMap f =
  join . fmap f
