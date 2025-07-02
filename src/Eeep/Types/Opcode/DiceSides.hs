{- |
Module: Eeep.Types.Opcode.DiceSides

The @DiceSides@ type.
-}

module Eeep.Types.Opcode.DiceSides (
    -- * Types.
    DiceSides (..)
) where

-- Imports.
-- Base.
import Data.Word (Word32)


{- | The 'DiceSides' type. -}
newtype DiceSides = DiceSides Word32
    deriving stock (Eq, Ord, Show)
