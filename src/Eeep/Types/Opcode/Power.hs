{-# LANGUAGE UndecidableInstances #-}

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
) where

-- Imports.
-- Base.
import Data.Functor.Contravariant (Contravariant (..))
import Data.Word (Word8)
import Data.Ix (Ix)

-- non-Hackage libraries.
import Trisagion.Utils.Either ((:+:))
import Trisagion.Typeclasses.Source (Source)
import Trisagion.Typeclasses.Sink (Sink)
import Trisagion.Parser (Parser)
import Trisagion.Parsers.Combinators (validate)
import Trisagion.Parsers.Source (InputError, one)
import Trisagion.Serializer (Serializer)
import Trisagion.Serializers.Binary (Binary (word8))

-- Package.
import Eeep.Typeclasses.Binary (Reader (..), Writer (..))
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

instance Source Word8 s => Reader s (PowerError :+: InputError) Power where
    {-# INLINE parser #-}
    parser :: Parser s (PowerError :+: InputError) Power
    parser = validate power one

instance (Sink Word8 b s, Binary b s) => Writer b s Power where
    {-# INLINE serializer #-}
    serializer :: Serializer s Power
    serializer = contramap unwrap word8
        where
            unwrap :: Power -> Word8
            unwrap (Power n) = n


{- | Smart constructor for the @t'Power'@ type. -}
{-# INLINE power #-}
power :: Word8 -> PowerError :+: Power
power n = eitherEnum (PowerError n) n
