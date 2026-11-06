{- |
Module: Eeep.Types.Shared.Strref

The @Strref@ string reference type.
-}

module Eeep.Types.Shared.Strref (
    -- * Types.
    Strref (..),

    -- ** Prisms.
    strref,

    -- ** Parsers and serializers.
    decode,
    encode,
) where

-- Imports.
-- Base.
import Data.Functor.Contravariant (Contravariant (..))
import Data.Ix (Ix)
import Data.Word (Word32)

-- Libraries.
import Optics.Core (Iso', coercedTo, view, review)

-- non-Hackage libraries.
import qualified Trisagion.Typeclasses.Parsers.Binary as Parsers (Binary (..))
import Trisagion.Parser (Parser)
import Trisagion.Parsers.Source (InputError)
import qualified Trisagion.Typeclasses.Serializers.Binary as Serializers (Binary (..))
import Trisagion.Serializer (Serializer)


{- | A .tlk string reference.

A .tlk string reference can only be dereferenced or validated in the IO monad.
-}
newtype Strref = Strref Word32
    deriving stock (Eq, Ord, Bounded, Ix)
    deriving newtype (Enum, Show)


{- | Isomorphism for the t'Strref' type. -}
{-# INLINE strref #-}
strref :: Iso' Strref Word32
strref = coercedTo


{- | Decode a t'Strref'. -}
{-# INLINE decode #-}
decode :: Parsers.Binary b s => Parser s InputError Strref
decode = fmap (review strref) Parsers.word32Le

{- | Encode a t'Strref'. -}
{-# INLINE encode #-}
encode :: Serializers.Binary b s => Serializer s Strref
encode = contramap (view strref) Serializers.word32Le
