{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module: Eeep.Types.Shared.Dice

The @Dice@ type for dice rolls.

note(s):

    * No custom parser and serializer is provided because in concrete examples (e. g. spells, items
    , etc.) the fields are not consecutive and the space in-between varies.
-}

module Eeep.Types.Shared.Dice (
    -- * Types.
    Dice,

    -- ** Isomorphisms.
    dice,
) where

-- Imports.
-- Base.
import GHC.Generics (Generic)
import Data.Word (Word8)

-- Libraries.
import Optics.Core (Iso', iso)


{- | The @Dice@ type. -}
data Dice = Dice {
    number :: {-# UNPACK #-} !Word8,
    sides  :: {-# UNPACK #-} !Word8
} deriving stock (Eq, Ord, Bounded, Show, Generic)


{- | Isomorphism for the t'Dice' type. -}
{-# INLINE dice #-}
dice :: Iso' Dice (Word8, Word8)
dice = iso from to
    where
        from :: Dice -> (Word8, Word8)
        from (Dice m n) = (m, n)

        to :: (Word8, Word8) -> Dice
        to (number, sides) = Dice {..}
