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
import Trisagion.Parser (Parser)
import Trisagion.Parsers.IStream (InputError)
import qualified Trisagion.Parsers.Binary as Parsers (Binary (..))
import Trisagion.Serializer (Serializer)
import qualified Trisagion.Serializers.Binary as Serializers (Binary (..))


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
encode :: Serializers.Binary s => Serializer s Strref
encode = contramap (view strref) Serializers.word32Le
