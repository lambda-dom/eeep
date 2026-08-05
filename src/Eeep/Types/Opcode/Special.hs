{- |
Module: Eeep.Types.Opcode.Special

The @Special@ type.
-}

module Eeep.Types.Opcode.Special (
    -- * Types.
    Special,

    -- ** Isomorphisms.
    special,
) where

-- Imports.
-- Base.
import Data.Ix (Ix)
import Data.Word (Word32)

-- Libraries.
import Optics.Core (Iso', coercedTo)


{- | The t'Special' type. -}
newtype Special = Special Word32
    deriving stock (Eq, Ord, Bounded, Ix, Show)
    deriving newtype Enum


{- | Isomorphism for the t'Special' type. -}
{-# INLINABLE special #-}
special :: Iso' Special Word32
special = coercedTo
