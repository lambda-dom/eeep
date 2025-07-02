{- |
Module: Eeep.Types.Effect.Projectile

The @Projectile@ type.
-}

module Eeep.Types.Effect.Projectile (
    -- * Types.
    Projectile (..),
) where

-- Imports.
-- Base.
import Data.Word (Word32)


{- | The t'Projectile' numeric id type. -}
newtype Projectile = Projectile Word32
    deriving stock (Eq, Show)
