{-# LANGUAGE NoFieldSelectors #-}

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
import Data.Word (Word8, Word32)

-- Libraries.
import Optics.Core (Prism', prism', review, view)

-- Package.
import Eeep.Types.Opcode.Duration (Seconds, Ticks, seconds, ticks)


{- | The @Timing@ coproduct type. -}
data Timing
    = Limited          !Seconds
    | Instant
    | Equipped
    | DelayedLimited   !Seconds
    | DelayedInstant   !Seconds
    | DelayedEquipped  !Seconds
    | DurationLimited  !Ticks
    | DurationInstant  !Ticks
    | DurationEquipped !Ticks
    | Permanent
    | InstantLimited   !Ticks
    deriving stock (Eq, Ord, Show)


{- | The prism for the t'Timing' type. -}
{-# INLINABLE timing #-}
timing :: Prism' (Word8, Word32) Timing
timing = prism' construct match
    where
        construct :: Timing -> (Word8, Word32)
        construct (Limited          d) = (0, view seconds d)
        construct Instant              = (1, 0)
        construct Equipped             = (2, 0)
        construct (DelayedLimited   d) = (3, view seconds d)
        construct (DelayedInstant   d) = (4, view seconds d)
        construct (DelayedEquipped  d) = (5, view seconds d)
        construct (DurationLimited  t) = (6, view ticks t)
        construct (DurationInstant  t) = (7, view ticks t)
        construct (DurationEquipped t) = (8, view ticks t)
        construct Permanent            = (9, 0)
        construct (InstantLimited   t) = (10, view ticks t)

        match :: (Word8, Word32) -> Maybe Timing
        match (0, n)  = Just $ Limited (review seconds n)
        match (1, _)  = Just $ Instant
        match (2, _)  = Just $ Equipped
        match (3, n)  = Just $ DelayedLimited (review seconds n)
        match (4, n)  = Just $ DelayedInstant (review seconds n)
        match (5, n)  = Just $ DelayedEquipped (review seconds n)
        match (6, n)  = Just $ DurationLimited (review ticks n)
        match (7, n)  = Just $ DurationInstant (review ticks n)
        match (8, n)  = Just $ DurationEquipped (review ticks n)
        match (9, _)  = Just $ Permanent
        match (10, n) = Just $ InstantLimited (review ticks n)
        match (_, _)  = Nothing 
