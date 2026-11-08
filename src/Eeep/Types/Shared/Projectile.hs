{- |
Module: Eeep.Types.Shared.Projectile

The @Projectile@ type.
-}

module Eeep.Types.Shared.Projectile (
    -- * Types.
    Projectile (..),

    -- ** Isomorphisms.
    projectile,

    -- ** Parsers and serializers.
    decode,
    encode,
) where

-- Imports.
-- Base.
import Data.Functor.Contravariant (Contravariant (..))
import Data.Ix (Ix)
import Data.Word (Word16)

-- Libraries.
import Optics.Core (Iso', coercedTo, view, review)

-- non-Hackage libraries.
import qualified Trisagion.Typeclasses.Parsers.Binary as Parsers (Binary (..))
import Trisagion.Parser (Parser)
import Trisagion.Parsers.Source (InputError)
import qualified Trisagion.Typeclasses.Serializers.Binary as Serializers (Binary (..))
import Trisagion.Serializer (Serializer)


{- | The t'Projectile' numeric id type.

note(s):

    * t'Projectile' values can only be validated in the 'IO' monad.
-}
newtype Projectile = Projectile Word16
    deriving stock (Eq, Ord, Bounded, Ix)
    deriving newtype (Enum, Show)


{- | Isomorphism for the t'Projectile' type. -}
{-# INLINE projectile #-}
projectile :: Iso' Projectile Word16
projectile = coercedTo


{- | Decode a t'Projectile'. -}
{-# INLINE decode #-}
decode :: Parsers.Binary b s => Parser s InputError Projectile
decode = fmap (review projectile) Parsers.word16Le

{- | Encode a t'Projectile'. -}
{-# INLINE encode #-}
encode :: Serializers.Binary b s => Serializer s Projectile
encode = contramap (view projectile) Serializers.word16Le
