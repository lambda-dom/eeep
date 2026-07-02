{- |
Module: Eeep.Utils.Bits

Utilities for types @a@ with a @'Bits' a@ constraint.
-}

module Eeep.Utils.Bits (
    -- * 'Bits' utilities.
    bitAt,
) where

-- Imports.
-- Base.
import Data.Bits (Bits (..))

-- Libraries.
import Optics.Core (Lens')
import Optics.Lens (lens)


{- | Generic flag lens. -}
{-# INLINE bitAt #-}
bitAt :: forall a . Bits a => Int -> Lens' a Bool
bitAt i = lens project update
    where
        project :: a -> Bool
        project n = testBit n i

        update :: a -> Bool -> a
        update n b = if b then setBit n i else clearBit n i

