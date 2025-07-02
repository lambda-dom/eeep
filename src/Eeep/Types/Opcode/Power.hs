{- |
Module: Eeep.Types.Opcode.Power

The @Power@ type.
-}

module Eeep.Types.Opcode.Power (
    -- * Types.
    Power,

    -- ** Constructors.
    toPower,
) where

-- Imports.
-- Base.
import Data.Word (Word8)
import Data.Ix (Ix)


{- | Refinement type containing values in the interval @[0 .. 10]@. -}
newtype Power = Power Word8
    deriving stock (Eq, Ord, Ix, Show)

-- Instances.
instance Bounded Power where
    {-# INLINE minBound #-}
    minBound :: Power
    minBound = Power 0

    {-# INLINE maxBound #-}
    maxBound :: Power
    maxBound = Power 10


{- | Smart constructor for the 'Power' type.-}
{-# INLINE toPower #-}
toPower :: Int -> Maybe Power
toPower n = if 0 <= n && n <= 10 then Just $ Power (fromIntegral n) else Nothing
