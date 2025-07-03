{- |
Module: Eeep.Types.Opcode.Target

The @Target@ type.
-}

module Eeep.Types.Opcode.Target (
    -- * Types.
    Target,

    -- ** Constructors.
    toTarget,
) where

-- Imports.
-- Base.
import Data.Ix (Ix)
import Data.Word (Word32)


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


{- | Smart constructor for the 'Target' type.-}
{-# INLINE toTarget #-}
toTarget :: Word32 -> Maybe Target
toTarget n = if m <= fromEnum (maxBound @Target) then Just $ toEnum m else Nothing
    where
        m = fromIntegral n
