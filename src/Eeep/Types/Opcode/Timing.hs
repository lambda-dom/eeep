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
import Trisagion.Utils.Either ((:+:), annotate)
import Trisagion.Parser (Parser)
import Trisagion.Parsers.Source (InputError)
import qualified Trisagion.Typeclasses.Parsers.Binary as Parsers (Binary (..))
import Trisagion.Parsers.Combinators (validate)
import qualified Trisagion.Typeclasses.Serializers.Binary as Serializers (Binary (..))
import Trisagion.Serializer (Serializer)


{- | The t'Timing' @Error@ type. -}
newtype Error = Error Word8
    deriving stock (Eq, Ord, Bounded, Show)
    deriving newtype Enum


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
decode :: Parsers.Binary b s => Parser s (Error :+: InputError) Timing
decode = validate (annotate Error (preview timing)) Parsers.word8

{- | Encode a t'Timing'. -}
{-# INLINE encode #-}
encode :: Serializers.Binary b s => Serializer s Timing
encode = contramap (review timing) Serializers.word8
