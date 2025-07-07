{- |
Module: Eeep.Types.Opcode.Timing

The @Timing@ type.
-}

module Eeep.Types.Opcode.Timing (
    -- * Error types.
    TimingError (..),

    -- * Types.
    Timing,

    -- ** Constructors.
    toTiming,

    -- * Parsers.
    parseTiming8,

    -- * Types.
    parseTiming16,
) where

-- Imports.
-- Base.
import Data.Bifunctor (Bifunctor (..))
import Data.Ix (Ix)
import Data.Void (absurd)
import Data.Word (Word8, Word16)

-- non-Hackage libraries.
import Mono.Typeclasses.MonoFunctor (ElementOf)
import Mono.Typeclasses.MonoFoldable (MonoFoldable)
import Trisagion.Types.ParseError (ParseError)
import Trisagion.Typeclasses.HasOffset (HasOffset)
import Trisagion.Typeclasses.Splittable (Splittable (PrefixOf))
import Trisagion.Parser (Parser)
import Trisagion.Parsers.ParseError (throwParseError, capture)
import Trisagion.Parsers.Word8 (word8, word16Le)


{- | The t'TimingError' type. -}
newtype TimingError = TimingError Word16
    deriving stock (Eq, Show)


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


{- | Smart constructor for the 'Timing' type.-}
{-# INLINE toTiming #-}
toTiming :: Word8 -> Maybe Timing
toTiming n = if m <= fromEnum (maxBound @Timing) then Just $ toEnum m else Nothing
    where
        m = fromIntegral n


{- | Parse a t'Timing' from a 'Word8'. -}
parseTiming8 :: (HasOffset s, ElementOf s ~ Word8) => Parser s (ParseError TimingError) Timing
parseTiming8 = capture $ do
    n <- first (fmap absurd) word8
    maybe (throwParseError . TimingError . fromIntegral $ n) pure (toTiming n)

{- | Parse a t'Timing' from a single 'Word16'. -}
parseTiming16
    :: (HasOffset s, Splittable s, MonoFoldable (PrefixOf s), ElementOf (PrefixOf s) ~ Word8)
    => Parser s (ParseError TimingError) Timing
parseTiming16 = capture $ do
        n <- first (fmap absurd) word16Le
        if n > upper
            then throwParseError . TimingError $ n
            else maybe (throwParseError . TimingError $ n) pure (toTiming (fromIntegral n))
    where
        upper = fromIntegral (maxBound @Word8)
