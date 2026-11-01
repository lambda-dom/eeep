{- |
Module: Eeep.Types.Opcode.SaveBonus

The @SaveBonus@ type.
-}

module Eeep.Types.Opcode.SaveBonus (
    -- * Types.
    SaveBonus,

    -- ** Prisms.
    saveBonus,
) where

-- Imports.
-- Base.
import Data.Int (Int32)
import Data.Ix (Ix)

-- Libraries.
import Optics.Core (Prism', prism', review, preview)

-- non-Hackage libraries.
import Data.Enum.Optics (enum)


{- | The @SaveBonus@ type. 

Refinement type to constrain the values of save bonuses to the interval @[-20 .. 20]@.
-}
newtype SaveBonus = SaveBonus Int32
    deriving stock (Eq, Ord, Ix, Show)
    deriving newtype Enum


-- Instances.
instance Bounded SaveBonus where
    {-# INLINE minBound #-}
    minBound :: SaveBonus
    minBound = SaveBonus (-20)

    {-# INLINE maxBound #-}
    maxBound :: SaveBonus
    maxBound = SaveBonus 20


{- | Smart constructor for the @t'SaveBonus'@ type. -}
{-# INLINABLE saveBonus #-}
saveBonus :: Prism' Int32 SaveBonus
saveBonus = prism' construct match
    where
        construct :: SaveBonus -> Int32
        construct = fromIntegral @Int @Int32 . review enum

        match :: Int32 -> Maybe SaveBonus
        match = preview enum . fromIntegral @Int32 @Int
