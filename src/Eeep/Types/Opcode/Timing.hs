{-# LANGUAGE UndecidableInstances #-}

{- |
Module: Eeep.Types.Opcode.Timing

The @Timing@ type.
-}

module Eeep.Types.Opcode.Timing (
    -- * Error types.
    TimingError (..),

    -- * Types.
    Timing (..),

    -- ** Constructors.
    timing,
) where

-- Imports.
-- Base.
import Data.Functor.Contravariant (Contravariant (..))
import Data.Ix (Ix)
import Data.Word (Word8)

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
import Eeep.Utils.Enum (eitherEnum)
import Eeep.Typeclasses.Binary (Reader (..), Writer (..))


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


-- Instances.
instance Source Word8 s => Reader s (TimingError :+: InputError) Timing where
    {-# INLINE parser #-}
    parser :: Parser s (TimingError :+: InputError) Timing
    parser = validate timing one

instance (Sink Word8 b s, Binary b s) => Writer b s Timing where
    {-# INLINE serializer #-}
    serializer :: Serializer s Timing
    serializer = contramap (fromIntegral . fromEnum) word8


{- | Smart constructor for the @t'Timing'@ type. -}
{-# INLINE timing #-}
timing :: Word8 -> TimingError :+: Timing
timing n = eitherEnum (TimingError n) n
