{- |
Module: Eeep.Types.Effect.Projectile

The @Projectile@ type.
-}

module Eeep.Types.Effect.Projectile (
    -- * Types.
    Projectile (..),

    -- * Parsers.
    parseProjectile,
) where

-- Imports.
-- Base.
import Data.Word (Word8, Word32)

-- non-Hackage libraries.
import Mono.Typeclasses.MonoFunctor (ElementOf)
import Mono.Typeclasses.MonoFoldable (MonoFoldable)
import Trisagion.Typeclasses.HasOffset (HasOffset)
import Trisagion.Typeclasses.Splittable (Splittable (PrefixOf))
import Trisagion.Parser (Parser)
import Trisagion.Parsers.ParseError (capture)
import Trisagion.Parsers.Streamable (InputError)
import Trisagion.Parsers.Word8 (word32Le)


{- | The t'Projectile' numeric id type. -}
newtype Projectile = Projectile Word32
    deriving stock (Eq, Show)


{- | Parse a t'Projectile' numeric id. -}
parseProjectile
    :: (HasOffset s, Splittable s, MonoFoldable (PrefixOf s), ElementOf (PrefixOf s) ~ Word8)
    => Parser s InputError Projectile
parseProjectile = capture . fmap Projectile $ word32Le
