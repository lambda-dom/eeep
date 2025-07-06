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
) where

-- Imports.
-- Base.
import Data.Bifunctor (Bifunctor (..))
import Data.Ix (Ix)
import Data.Void (absurd)
import Data.Word (Word8)

-- non-Hackage libraries.
import Mono.Typeclasses.MonoFunctor (ElementOf)
import Trisagion.Types.ParseError (ParseError)
import Trisagion.Typeclasses.HasOffset (HasOffset)
import Trisagion.Parser (Parser)
import Trisagion.Parsers.ParseError (throwParseError, capture)
import Trisagion.Parsers.Word8 (word8)

-- Package.
import Eeep.Typeclasses.Binary (Reader (..))


{- | The t'TimingError' type. -}
newtype TimingError = TimingError Word8
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


-- Instances
instance (HasOffset s, ElementOf s ~ Word8) => Reader s TimingError Timing where
    parser :: Parser s (ParseError TimingError) Timing
    parser = capture $ do
        n <- first (fmap absurd) word8
        maybe (throwParseError $ TimingError n) pure (toTiming n)


{- | Smart constructor for the 'Timing' type.-}
{-# INLINE toTiming #-}
toTiming :: Word8 -> Maybe Timing
toTiming n = if m <= fromEnum (maxBound @Timing) then Just $ toEnum m else Nothing
    where
        m = fromIntegral n
