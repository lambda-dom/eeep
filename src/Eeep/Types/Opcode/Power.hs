{- |
Module: Eeep.Types.Opcode.Power

The @Power@ type.
-}

module Eeep.Types.Opcode.Power (
    -- * Error types.
    Error (..),

    -- * Types.
    Power,

    -- ** Prisms.
    power,

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


{- | The t'Power' @Error@ type. -}
newtype Error = Error Word8
    deriving stock (Eq, Ord, Bounded, Ix, Show)
    deriving newtype Enum


{- | The @Power@ type.

A refinement type containing the values in the interval @[0 .. 10]@.
-}
newtype Power = Power Word8
    deriving stock (Eq, Ord, Ix)
    deriving newtype (Enum, Show)


-- Instances.
instance Bounded Power where
    {-# INLINE minBound #-}
    minBound :: Power
    minBound = Power 0

    {-# INLINE maxBound #-}
    maxBound :: Power
    maxBound = Power 10


{- | Prism for the @t'Power'@ type. -}
{-# INLINE power #-}
power :: Prism' Word8 Power
power = prism' construct match
    where
        construct :: Power -> Word8
        construct = fromIntegral @Int @Word8 . review enum

        match :: Word8 -> Maybe Power
        match = preview enum . review word8ToInt


{- | Decode a t'Power'. -}
{-# INLINE decode #-}
decode :: Parsers.Binary b s => Parser s (InputError :+: Error) Power
decode = validate (validator Error (preview power)) Parsers.word8

{- | Encode a t'Power'. -}
{-# INLINE encode #-}
encode :: Serializers.Binary s => Serializer s Power
encode = contramap (review power) Serializers.word8
