{- |
Module: Eeep.Types.Opcode.ResistDispel

The @ResistDispel@ type.
-}

module Eeep.Types.Opcode.ResistDispel (
    -- * Types.
    ResistDispel,

    -- ** Constructors.
    toResistDispel,
) where

-- Imports.
-- Base.
import Data.Ix (Ix)


{- | The @ResistDispel@ enumeration type. -}
data ResistDispel
    = Natural
    | DispellableResistable
    | UndispellableUnresistable
    | DispellableUnresistable
    deriving stock (Eq, Ord, Enum, Bounded, Ix, Show)


{- | Smart constructor for the 'ResistDispel' type.-}
{-# INLINE toResistDispel #-}
toResistDispel :: Int -> Maybe ResistDispel
toResistDispel n = if 0 <= n && n <= 3 then Just $ toEnum n else Nothing
