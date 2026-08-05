{- |
Module: Eeep.Types.Opcode.Target

The @Target@ type.
-}

module Eeep.Types.Opcode.Target (
    -- * Types.
    Target (..),

    -- ** Prisms.
    target,
) where

-- Imports.
-- Base.
import Data.Ix (Ix)
import Data.Word (Word8)

-- Libraries.
import Optics.Prism (Prism')

-- Package.
import Eeep.Utils.Enum (enum)


{- | The @Target@ enumeration type. -}
data Target
    = None
    | Self
    | Preset
    | Party
    | Area
    | NotParty
    | CasterGroup
    | TargetGroup
    | NotSelf
    | Original
    deriving stock (Eq, Ord, Enum, Bounded, Ix, Show)


{- | Prism for the @t'Target'@ type. -}
{-# INLINABLE target #-}
target :: Prism' Word8 Target
target = enum
