{- |
Module: Eeep.Types.Opcode.Duration

The @Duration@ type.
-}

module Eeep.Types.Opcode.Duration (
    -- * Types.
    Duration (..)
) where

-- Imports.
-- Base.
import Data.Word (Word32)


{- | The 'Duration' type. -}
newtype Duration = Duration Word32
    deriving stock (Eq, Ord, Show)
