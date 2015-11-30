module BinaryParser.Prelude
( 
  module Exports,
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


{-# INLINE mapLeft #-}
mapLeft :: (a -> b) -> Either a x -> Either b x
mapLeft f =
  either (Left . f) Right

joinMap :: Monad m => (a -> m b) -> m a -> m b
joinMap f =
  join . liftM f
