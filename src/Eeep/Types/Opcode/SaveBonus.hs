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
import Optics.Core (Prism')

-- Package.
import Eeep.Utils.Enum (enum)


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
saveBonus = enum
