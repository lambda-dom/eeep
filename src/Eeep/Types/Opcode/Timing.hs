{- |
Module: Eeep.Types.Opcode.Timing

The @Timing@ type.
-}

module Eeep.Types.Opcode.Timing (
    -- * Error types.
    Error (..),

    -- * Types.
    Timing (..),

    -- ** Prisms.
    timing,

    -- ** Parsers and serializers.
    decode,
    encode,
) where

-- Imports.
-- Base.
import Data.Functor.Contravariant (Contravariant(..))
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


{- | The t'Timing' @Error@ type. -}
newtype Error = Error Word8
    deriving stock (Eq, Ord, Bounded, Show)
    deriving newtype Enum


{- | The @Timing@ enumeration type for the timing mode of an opcode. -}
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
        match = preview enum . review word8ToInt


{- | Decode a t'Timing'. -}
{-# INLINE decode #-}
decode :: Parsers.Binary b s => Parser s (InputError :+: Error) Timing
decode = validate (validator Error (preview timing)) Parsers.word8

{- | Encode a t'Timing'. -}
{-# INLINE encode #-}
encode :: Serializers.Binary s => Serializer s Timing
encode = contramap (review timing) Serializers.word8
