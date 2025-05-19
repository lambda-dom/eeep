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
toTiming :: Int -> Maybe Timing
toTiming n = if 0 <= n && n <= toEnum maxBound then Just (toEnum n) else Nothing
