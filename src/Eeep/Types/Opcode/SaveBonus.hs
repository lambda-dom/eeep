{- |
Module: Eeep.Types.Opcode.SaveBonus

The @SaveBonus@ type.
-}

module Eeep.Types.Opcode.SaveBonus (
    -- * Types.
    SaveBonus,

    -- ** Constructors.
    toSaveBonus,
) where

-- Imports.
-- Base.
import Data.Int (Int8)
import Data.Ix (Ix)


{- | The @SaveBonus@ type. 

Refinement type to constrain the values of save bonuses to the interval @[-20 .. 20]@.
-}
newtype SaveBonus = SaveBonus Int8
    deriving stock (Eq, Ord, Ix, Show)

-- Instances.
instance Bounded SaveBonus where
    {-# INLINE minBound #-}
    minBound :: SaveBonus
    minBound = SaveBonus (-20)

    {-# INLINE maxBound #-}
    maxBound :: SaveBonus
    maxBound = SaveBonus 20


{- | Smart constructor for the 'SaveBonus' type.-}
{-# INLINE toSaveBonus #-}
toSaveBonus :: Int -> Maybe SaveBonus
toSaveBonus n = if -20 <= n && n <= 20 then Just $ SaveBonus (fromIntegral n) else Nothing
