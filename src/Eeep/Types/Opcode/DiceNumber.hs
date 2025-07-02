{- |
Module: Eeep.Types.Opcode.DiceNumber

The @DiceNumber@ type.
-}

module Eeep.Types.Opcode.DiceNumber (
    -- * Types.
    DiceNumber (..)
) where

-- Imports.
-- Base.
import Data.Word (Word32)


{- | The 'DiceNumber' type. -}
newtype DiceNumber = DiceNumber Word32
    deriving stock (Eq, Ord, Show)
