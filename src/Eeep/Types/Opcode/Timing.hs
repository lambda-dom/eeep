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


{- | The @Timing@ GADT type. -}
data Timing where
    Limited          :: !Seconds -> Timing
    Instant          :: Timing
    Equipped         :: Timing
    DelayedLimited   :: !Seconds -> Timing
    DelayedInstant   :: !Seconds -> Timing
    DelayedEquipped  :: !Seconds -> Timing
    DurationLimited  :: !Ticks -> Timing
    DurationInstant  :: !Ticks -> Timing
    DurationEquipped :: !Ticks -> Timing
    Permanent        :: Timing
    InstantLimited   :: !Ticks -> Timing
    deriving stock (Eq, Ord)


{- | The prism for the t'Timing' type. -}
{-# INLINABLE timing #-}
timing :: Prism' (Word8, Word32) Timing
timing = prism' construct match
    where
        construct :: Timing -> (Word8, Word32)
        construct = \case
            Limited          d -> (0, view seconds d)
            Instant            -> (1, 0)
            Equipped           -> (2, 0)
            DelayedLimited   d -> (3, view seconds d)
            DelayedInstant   d -> (4, view seconds d)
            DelayedEquipped  d -> (5, view seconds d)
            DurationLimited  t -> (6, view ticks t)
            DurationInstant  t -> (7, view ticks t)
            DurationEquipped t -> (8, view ticks t)
            Permanent          -> (9, 0)
            InstantLimited   t -> (10, view ticks t)

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
