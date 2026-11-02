{- |
Module: Eeep.Types.Opcode.Timing

The @Timing@ type.
-}

module Eeep.Types.Opcode.Timing (
    -- * Error types.
    TimingError (..),

    -- * Types.
    Timing (..),

    -- ** Prisms.
    timing,

    -- ** Parsers.
    decode,
) where

-- Imports.
-- Base.
import Data.Ix (Ix)
import Data.Word (Word8)

-- Libraries.
import Optics.Core (Prism', prism', review, preview)

-- non-Hackage libraries.
import Data.Enum.Optics (enum)
import Trisagion.Utils.Either ((:+:), annotate)
import Trisagion.Parser (Parser)
import Trisagion.Parsers.Source (InputError)
import Trisagion.Typeclasses.Parsers.Binary (Binary (..))
import Trisagion.Parsers.Combinators (validate)


{- | The @TimingError@ type. -}
newtype TimingError = TimingError Word8
    deriving stock (Eq, Ord, Bounded, Show)


{- | The @Timing@ enumeration type. -}
data Timing
    = Limited
    | Instant
    | Equipped
    | DelayedLimited
    | DelayedInstant
    | DelayedEquipped
    | DurationLimited
    | DurationInstant
    | DurationEquipped
    | Permanent
    | InstantLimited
    deriving stock (Eq, Ord, Enum, Bounded, Ix, Show)


{- | The prism for the t'Timing' type. -}
{-# INLINE timing #-}
timing :: Prism' Word8 Timing
timing = prism' construct match
    where
        construct :: Timing -> Word8
        construct = fromIntegral @Int @Word8 . review enum

        match :: Word8 -> Maybe Timing
        match = preview enum . fromIntegral @Word8 @Int


{- | Decode a t'Timing'. -}
{-# INLINE decode #-}
decode :: Binary b s => Parser s (TimingError :+: InputError) Timing
decode = validate (annotate TimingError (preview timing)) word8
