{- |
Module: Eeep.Types.Opcode.Parameter

The @Parameter@ type.
-}

module Eeep.Types.Opcode.Parameter (
    -- * Types.
    Parameter (..)
) where

-- Imports.
-- Base.
import Data.Word (Word32)


{- | The 'Parameter' type. -}
newtype Parameter = Parameter Word32
    deriving stock (Eq, Ord, Show)
