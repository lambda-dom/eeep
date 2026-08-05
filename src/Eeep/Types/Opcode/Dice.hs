{-# LANGUAGE NoFieldSelectors #-}

{- |
Module: Eeep.Types.Opcode.Dice

The @Dice@ type.
-}

module Eeep.Types.Opcode.Dice (
    -- * Types.
    Dice,

    -- ** Isomorphisms.
    dice,
) where

-- Imports.
-- Base.
import Data.Word (Word32)
import GHC.Generics (Generic)

-- Libraries.
import Optics.Core (Iso', iso)


{- | The @Dice@ type. -}
data Dice = Dice {
    number :: {-# UNPACK #-} !Word32,
    sides  :: {-# UNPACK #-} !Word32
    } deriving stock (Eq, Ord, Generic, Show)


{- | Isomorphism for the t'Dice' type. -}
{-# INLINABLE dice #-}
dice :: Iso' Dice (Word32, Word32)
dice = iso from to
    where
        from :: Dice -> (Word32, Word32)
        from (Dice m n) = (m, n)

        to :: (Word32, Word32) -> Dice
        to (m, n) = Dice m n
