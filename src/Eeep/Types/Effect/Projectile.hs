{- |
Module: Eeep.Types.Effect.Projectile

The @Projectile@ type.
-}

module Eeep.Types.Effect.Projectile (
    -- * Types.
    Projectile (..),

    -- * Parsers.
    decodeProjectile,

    -- * Serializers.
    encodeProjectile,
) where

-- Imports.
-- Base.
import Data.Functor.Contravariant (Contravariant (..))
import Data.Word (Word8, Word32)

-- non-Hackage libraries.
import Mono.Typeclasses.MonoFunctor (ElementOf)
import Mono.Typeclasses.MonoFoldable (MonoFoldable)
import Trisagion.Typeclasses.HasOffset (HasOffset)
import Trisagion.Typeclasses.Splittable (Splittable (PrefixOf))
import Trisagion.Typeclasses.Binary (Binary)
import Trisagion.Parser (Parser)
import Trisagion.Parsers.ParseError (capture)
import Trisagion.Parsers.Streamable (InputError)
import Trisagion.Parsers.Word8 (word32Le)
import Trisagion.Serializer (Serializer)
import qualified Trisagion.Serializers.Binary as Binary (word32Le)


{- | The t'Projectile' numeric id type. -}
newtype Projectile = Projectile Word32
    deriving stock (Eq, Show)


{- | Decode a t'Projectile' numeric id. -}
decodeProjectile
    :: (HasOffset s, Splittable s, MonoFoldable (PrefixOf s), ElementOf (PrefixOf s) ~ Word8)
    => Parser s InputError Projectile
decodeProjectile = capture . fmap Projectile $ word32Le


{- | Encode a t'Projectile' into a 'Word32'. -}
encodeProjectile :: Binary m => Serializer m Projectile
encodeProjectile = contramap unwrap Binary.word32Le
    where
        unwrap :: Projectile -> Word32
        unwrap (Projectile n) = n
