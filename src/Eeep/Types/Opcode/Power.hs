{- |
Module: Eeep.Types.Opcode.Power

The @Power@ type.
-}

module Eeep.Types.Opcode.Power (
    -- * Types.
    Power,

    -- ** Prisms.
    power,
) where

-- Imports.
-- Base.
import Data.Word (Word8)
import Data.Ix (Ix)

-- Libraries.
import Optics.Core (Prism')

-- Package.
import Eeep.Utils.Enum (enum)


{- | The @Power@ type.

A refinement type containing the values in the interval @[0 .. 10]@.
-}
newtype Power = Power Word8
    deriving stock (Eq, Ord, Ix, Show)
    deriving newtype Enum


-- Instances.
instance Bounded Power where
    {-# INLINE minBound #-}
    minBound :: Power
    minBound = Power 0

    {-# INLINE maxBound #-}
    maxBound :: Power
    maxBound = Power 10


{- | Prism for the @t'Power'@ type. -}
{-# INLINABLE power #-}
power :: Prism' Word8 Power
power = enum
