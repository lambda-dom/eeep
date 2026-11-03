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

    -- ** Parsers.
    decode,
) where

-- Imports.
-- Base.
import Data.Word (Word8)
import Data.Ix (Ix)

-- Libraries.
import Optics.Core (Prism', prism', review, preview)
import Data.Enum.Optics (enum)

-- non-Hackage libraries.
import Trisagion.Utils.Either ((:+:), annotate)
import Trisagion.Typeclasses.Parsers.Binary (Binary (..))
import Trisagion.Parser (Parser)
import Trisagion.Parsers.Combinators (validate)
import Trisagion.Parsers.Source (InputError)


{- | The t'Power' @Error@ type. -}
newtype Error = Error Word8
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


{- | Prism for the @t'Power'@ type. -}
{-# INLINE power #-}
power :: Prism' Word8 Power
power = prism' construct match
    where
        construct :: Power -> Word8
        construct = fromIntegral @Int @Word8 . review enum

        match :: Word8 -> Maybe Power
        match = preview enum . fromIntegral @Word8 @Int


{- | Decode a t'Power'. -}
{-# INLINE decode #-}
decode :: Binary b s => Parser s (Error :+: InputError) Power
decode = validate (annotate Error (preview power)) word8
