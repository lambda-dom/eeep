{- |
Module: Eeep.Types.Opcode.Duration

The @Duration@ type.
-}

module Eeep.Types.Opcode.Duration (
    -- * Types.
    Duration,

    -- ** Isomorphisms.
    duration,
) where

-- Imports.
-- Base.
import Data.Ix (Ix)
import Data.Word (Word32)

-- Libraries.
import Optics.Core (Iso', coercedTo)


{- | The t'Duration' type. -}
newtype Duration = Duration Word32
    deriving stock (Eq, Ord, Bounded, Ix, Show)
    deriving newtype Enum


{- | Isomorphism for the t'Duration' type. -}
{-# INLINE duration #-}
duration :: Iso' Duration Word32
duration = coercedTo
