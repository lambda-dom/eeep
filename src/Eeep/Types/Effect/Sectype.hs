{- |
Module: Eeep.Types.Effect.Sectype

The @Sectype@ type.
-}

module Eeep.Types.Effect.Sectype (
    -- * Types.
    Sectype (..),
) where

-- Imports.
-- Base.
import Data.Word (Word32)


{- | The magic t'Sectype' numeric id type. -}
newtype Sectype = Sectype Word32
    deriving stock (Eq, Show)
