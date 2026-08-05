{- |
Module: Eeep.Types.Shared.Percentage

The @Percentage@ type.
-}

module Eeep.Types.Shared.Percentage (
    -- * Types.
    Percentage,

    -- ** Isomorphisms.
    percentage,
) where

-- Imports.
-- Base.
import Data.Ix (Ix)
import Data.Word (Word32)

-- Libraries.
import Optics.Core (Iso', coercedTo)


{- | The t'Percentage' type. -}
newtype Percentage = Percentage Word32
    deriving stock (Eq, Ord, Bounded, Ix, Show)
    deriving newtype Enum


{- | Isomorphism for the t'Percentage' type. -}
{-# INLINABLE percentage #-}
percentage :: Iso' Percentage Word32
percentage = coercedTo
