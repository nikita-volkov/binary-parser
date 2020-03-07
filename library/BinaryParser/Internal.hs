module BinaryParser.Internal where

import BinaryParser.Prelude hiding (fold)

-- |
-- A highly-efficient parser specialised for strict 'ByteString's.
--
-- Supports the roll-back and alternative branching
-- on the basis of the 'Alternative' interface.
--
-- Does not generate fancy error-messages,
-- which contributes to its efficiency.
newtype BinaryParser a =
  BinaryParser ( StateT ByteString ( Except Text ) a )
  deriving ( Functor , Applicative , Alternative , Monad , MonadPlus , MonadError Text )
