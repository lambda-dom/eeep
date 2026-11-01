{- |
Module: Eeep.Types.Opcode.Timing

The @Timing@ type.
-}

module Eeep.Types.Opcode.Timing (
    -- * Types.
    Timing (..),

    -- ** Prisms.
    timing,
) where

-- Imports.
-- Base.
import Data.Ix (Ix)

-- Libraries.
import Optics.Core (Prism')

-- non-Hackage libraries.
import Data.Enum.Optics (enum)


{- | The @Timing@ GADT type. -}
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
    deriving stock (Eq, Ord, Enum, Bounded, Ix)


{- | The prism for the t'Timing' type. -}
{-# INLINE timing #-}
timing :: Prism' Int Timing
timing = enum
