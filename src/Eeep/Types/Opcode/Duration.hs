{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module: Eeep.Types.Opcode.Duration

The @Duration@ type.
-}

module Eeep.Types.Opcode.Duration (
    -- * Error types.
    TimingError (..),

    -- * The t'Timing' type.
    Timing (..),

    -- ** Parsers and serializers.
    encodeTiming,
    decodeTiming,

    -- * The t'Duration' type.
    Duration (..),

    -- ** Parsers and serializers.
    encodeDuration,
    decodeDuration,
) where

-- Imports.
-- Base.
import Data.Bifunctor (Bifunctor (..))
import Data.Functor.Contravariant (Contravariant (..))
import Data.Ix (Ix)
import Data.Word (Word32, Word8)
import GHC.Generics (Generic)

-- Libraries.
import Optics.Core ((^.), review)

-- non-Hackage libraries.
import Trisagion.Utils.Either ((:+:))
import Trisagion.Parser (Parser)
import Trisagion.Parsers.Combinators (validate)
import Trisagion.Parsers.Source (InputError, one)
import qualified Trisagion.Parsers.Binary as Parsers (Binary, word32Le)
import Trisagion.Serializer (Serializer)
import qualified Trisagion.Serializers.Binary as Serializers (Binary, word8, word32Le)

-- Package.
import Eeep.Utils.Enum (eitherEnum, enum)


{- | The t'TimingError' type. -}
newtype TimingError = TimingError Word8
    deriving stock (Eq, Ord, Bounded, Ix, Show)
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


{- | The @Duration@ type. -}
data Duration = Duration {
    timing   :: {-# UNPACK #-} !Timing,
    duration :: {-# UNPACK #-} !Word32
    } deriving stock (Eq, Ord, Bounded, Show, Generic)


{- | Default parser for t'Timing'. -}
{-# INLINE encodeTiming #-}
encodeTiming :: Parsers.Binary b s => Parser s (TimingError :+: InputError) Timing
encodeTiming = validate (\ n -> eitherEnum (TimingError n) n) one

{- | Default serializer for t'Timing'. -}
{-# INLINE decodeTiming #-}
decodeTiming :: Serializers.Binary b s => Serializer s Timing
decodeTiming = contramap (review enum) Serializers.word8

{- | Default parser for t'Duration'. -}
{-# INLINE encodeDuration #-}
encodeDuration :: Parsers.Binary b s => Parser s (TimingError :+: InputError) Duration
encodeDuration = do
    timing   <- encodeTiming
    duration <- first Right Parsers.word32Le
    pure $ Duration {..}

{- | Default serializer for t'Duration'. -}
{-# INLINE decodeDuration #-}
decodeDuration :: Serializers.Binary b s => Serializer s Duration
decodeDuration
    = contramap (^. #timing) decodeTiming
    <> contramap (^. #duration) Serializers.word32Le
