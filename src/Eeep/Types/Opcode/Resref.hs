{- |
Module: Eeep.Types.Opcode.Resref

The @Resref@ type.
-}

module Eeep.Types.Opcode.Resref (
    -- * Types.
    Resref (..),
) where

-- Imports.
-- Base.
import Data.Ix (Ix)
import Data.Word (Word64)


{- | The t'Resref' type for resource references. -}
newtype Resref = Resref Word64
    deriving stock (Eq, Ord, Ix, Show)
