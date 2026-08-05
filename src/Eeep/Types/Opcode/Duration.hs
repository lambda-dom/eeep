{- |
Module: Eeep.Types.Opcode.Duration

The @Duration@ type.
-}

module Eeep.Types.Opcode.Duration (
    -- * Types.
    Seconds,
    Ticks,

    -- ** Isomorphisms.
    seconds,
    ticks,
) where

-- Imports.
-- Base.
import Data.Ix (Ix)
import Data.Word (Word32)

-- Libraries.
import Optics.Core (Iso', coercedTo)


{- | The t'Seconds' duration type. -}
newtype Seconds = Seconds Word32
    deriving stock (Eq, Ord, Bounded, Ix, Show)
    deriving newtype Enum


{- | The t'Ticks' duration type. -}
newtype Ticks = Ticks Word32
    deriving stock (Eq, Ord, Bounded, Ix, Show)
    deriving newtype Enum


{- | Isomorphism for the t'Seconds' type. -}
{-# INLINABLE seconds #-}
seconds :: Iso' Seconds Word32
seconds = coercedTo

{- | Isomorphism for the t'Seconds' type. -}
{-# INLINABLE ticks #-}
ticks :: Iso' Ticks Word32
ticks = coercedTo
