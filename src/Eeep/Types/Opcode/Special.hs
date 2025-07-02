{- |
Module: Eeep.Types.Opcode.Special

The @Special@ type.
-}

module Eeep.Types.Opcode.Special (
    -- * Types.
    Special (..)
) where

-- Imports.
-- Base.
import Data.Word (Word32)


{- | The t'Special' type. -}
newtype Special = Special Word32
    deriving stock (Eq, Ord, Show)
