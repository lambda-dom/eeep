{- |
Module: Eeep.Types.Opcode.Power

The @Power@ type.
-}

module Eeep.Types.Opcode.Power (
    -- * Types.
    Power,

    -- * Error types.
    PowerError (..),

    -- ** Constructors.
    power,

    -- ** Parsers and serializers.
    encodePower,
    decodePower,
) where

-- Imports.
-- Base.
import Data.Functor.Contravariant (Contravariant (..))
import Data.Word (Word8)
import Data.Ix (Ix)

-- non-Hackage libraries.
import Trisagion.Utils.Either ((:+:))
import Trisagion.Typeclasses.Source (Source)
import Trisagion.Parser (Parser)
import Trisagion.Parsers.Combinators (validate)
import Trisagion.Parsers.Source (InputError, one)
import Trisagion.Serializer (Serializer)
import Trisagion.Serializers.Binary (Binary (word8))

-- Package.
import Eeep.Utils.Enum (eitherEnum)


{- | The t'PowerError' type. -}
newtype PowerError = PowerError Word8
    deriving stock (Eq, Ord, Bounded, Ix, Show)
    deriving newtype Enum


{- | The @Power@ type.

A refinement type containing the values in the interval @[0 .. 10]@.
-}
newtype Power = Power Word8
    deriving stock (Eq, Ord, Ix, Show)
    deriving newtype Enum


-- Instances.
instance Bounded Power where
    {-# INLINE minBound #-}
    minBound :: Power
    minBound = Power 0

    {-# INLINE maxBound #-}
    maxBound :: Power
    maxBound = Power 10


{- | Smart constructor for the @t'Power'@ type. -}
{-# INLINE power #-}
power :: Word8 -> PowerError :+: Power
power n = eitherEnum (PowerError n) n


{- | Default parser for t'Power'. -}
{-# INLINE encodePower #-}
encodePower :: Source Word8 s => Parser s (PowerError :+: InputError) Power
encodePower = validate power one

{- | Default serializer for t'Power'. -}
{-# INLINE decodePower #-}
decodePower :: Binary b s => Serializer s Power
decodePower = contramap unwrap word8
    where
        unwrap :: Power -> Word8
        unwrap (Power n) = n
