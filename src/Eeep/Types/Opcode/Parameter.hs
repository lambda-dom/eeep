{- |
Module: Eeep.Types.Opcode.Parameter

The @Parameter@ type.
-}

module Eeep.Types.Opcode.Parameter (
    -- * Types.
    Parameter,

    -- ** Isomorphisms.
    parameter,
) where

-- Imports.
-- Base.
import Data.Ix (Ix)
import Data.Word (Word32)

-- Libraries.
import Optics.Core (Iso', coercedTo)


{- | The t'Parameter' type.

note(s):

    * Eventually, this type will be folded into a beefed up GADT @Opcode@ type.
-}
newtype Parameter = Parameter Word32
    deriving stock (Eq, Ord, Bounded, Ix, Show)
    deriving newtype Enum


{- | Isomorphism for the t'Parameter' type. -}
{-# INLINABLE parameter #-}
parameter :: Iso' Parameter Word32
parameter = coercedTo
