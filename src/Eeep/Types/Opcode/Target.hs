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
toTarget :: Int -> Maybe Target
toTarget n = if 0 <= n && n <= 9 then Just $ toEnum n else Nothing
