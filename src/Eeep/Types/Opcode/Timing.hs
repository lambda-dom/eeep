{- |
Module: Eeep.Types.Opcode.Timing

The @Timing@ type.
-}

module Eeep.Types.Opcode.Timing (
    -- * Types.
    Timing,

    -- ** Constructors.
    toTiming,
) where

-- Imports.
-- Base.
import Data.Ix (Ix)
import Data.Word (Word32)


{- | The @Timing@ enumeration type. -}
data Timing
    = Limited
    | Instant
    | Equipped
    | DelayedLimited
    | DelayedInstant
    | DelayedEquipped
    | DurationLimited
    | DurationInstant
    | DurationEquipped
    | Permanent
    | InstantLimited
    deriving stock (Eq, Ord, Enum, Bounded, Ix, Show)


{- | Smart constructor for the 'Timing' type.-}
{-# INLINE toTiming #-}
toTiming :: Word32 -> Maybe Timing
toTiming n = if m <= fromEnum (maxBound @Timing) then Just $ toEnum m else Nothing
    where
        m = fromIntegral n
