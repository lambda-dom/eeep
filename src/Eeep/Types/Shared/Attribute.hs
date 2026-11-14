{- |
Module: Eeep.Types.Shared.Attribute

The @Attribute@ type.
-}

module Eeep.Types.Shared.Attribute (
    -- * Error types.
    Error (..),

    -- * Types.
    Attribute,

    -- ** Prisms.
    attribute,

    -- ** Parsers and serializers.
    decode,
    encode,
) where

-- Imports.
-- Base.
import Data.Functor.Contravariant (Contravariant (..))
import Data.Ix (Ix)
import Data.Word (Word8)

-- Libraries.
import Optics.Core (Prism', prism', review, preview)

-- non-Hackage libraries.
import Data.Enum.Optics (enum)
import Data.Word.Optics (word8ToInt)
import Trisagion.Utils.Either ((:+:), validator)
import Trisagion.Parser (Parser)
import Trisagion.Parsers.Combinators (validate)
import Trisagion.Parsers.IStream (InputError)
import qualified Trisagion.Parsers.Binary as Parsers (Binary (..))
import Trisagion.Serializer (Serializer)
import qualified Trisagion.Serializers.Binary as Serializers (Binary (..))


{- | The t'Attribute' @Error@ type. -}
newtype Error = Error Word8
    deriving stock (Eq, Ord, Bounded, Ix, Show)
    deriving newtype Enum


{- | The stat @Attribute@ type.

A refinement type containing the values in the interval @[0 .. 25]@.
-}
newtype Attribute = Attribute Word8
    deriving stock (Eq, Ord, Ix)
    deriving newtype (Enum, Show)


-- Instances.
instance Bounded Attribute where
    {-# INLINE minBound #-}
    minBound :: Attribute
    minBound = Attribute 0

    {-# INLINE maxBound #-}
    maxBound :: Attribute
    maxBound = Attribute 25


{- | Prism for the @t'Attribute'@ type. -}
{-# INLINE attribute #-}
attribute :: Prism' Word8 Attribute
attribute = prism' construct match
    where
        construct :: Attribute -> Word8
        construct = fromIntegral @Int @Word8 . review enum

        match :: Word8 -> Maybe Attribute
        match = preview enum . review word8ToInt


{- | Decode an t'Attribute'. -}
{-# INLINE decode #-}
decode :: Parsers.Binary b s => Parser s (InputError :+: Error) Attribute
decode = validate (validator Error (preview attribute)) Parsers.word8

{- | Encode an t'Attribute'. -}
{-# INLINE encode #-}
encode :: Serializers.Binary s => Serializer s Attribute
encode = contramap (review attribute) Serializers.word8
