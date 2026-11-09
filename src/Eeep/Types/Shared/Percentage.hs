{- |
Module: Eeep.Types.Shared.Percentage

The @Percentage@ type.
-}

module Eeep.Types.Shared.Percentage (
    -- * Error types.
    Error (..),

    -- * Types.
    Percentage,

    -- ** Prisms.
    percentage,

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
import Data.Enum.Optics (enum)

-- non-Hackage libraries.
import Trisagion.Utils.Either ((:+:), annotate)
import qualified Trisagion.Typeclasses.Parsers.Binary as Parsers (Binary (..))
import Trisagion.Parser (Parser)
import Trisagion.Parsers.Combinators (validate)
import Trisagion.Parsers.Source (InputError)
import qualified Trisagion.Typeclasses.Serializers.Binary as Serializers (Binary (..))
import Trisagion.Serializer (Serializer)


{- | The t'Percentage' @Error@ type. -}
newtype Error = Error Word8
    deriving stock (Eq, Ord, Bounded, Ix, Show)
    deriving newtype Enum


{- | The @Percentage@ type.

A refinement type containing the values in the interval @[0 .. 100]@.
-}
newtype Percentage = Percentage Word8
    deriving stock (Eq, Ord, Ix)
    deriving newtype (Enum, Show)


-- Instances.
instance Bounded Percentage where
    {-# INLINE minBound #-}
    minBound :: Percentage
    minBound = Percentage 0

    {-# INLINE maxBound #-}
    maxBound :: Percentage
    maxBound = Percentage 100


{- | Prism for the @t'Percentage'@ type. -}
{-# INLINE percentage #-}
percentage :: Prism' Word8 Percentage
percentage = prism' construct match
    where
        construct :: Percentage -> Word8
        construct = fromIntegral @Int @Word8 . review enum

        match :: Word8 -> Maybe Percentage
        match = preview enum . fromIntegral @Word8 @Int


{- | Decode an t'Percentage'. -}
{-# INLINE decode #-}
decode :: Parsers.Binary b s => Parser s (Error :+: InputError) Percentage
decode = validate (annotate Error (preview percentage)) Parsers.word8

{- | Encode an t'Percentage'. -}
{-# INLINE encode #-}
encode :: Serializers.Binary b s => Serializer s Percentage
encode = contramap (review percentage) Serializers.word8
