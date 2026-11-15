{- |
Module: Eeep.Types.Opcode.Timing

The @Timing@ type.
-}

module Eeep.Types.Opcode.Timing (
    -- * Error types.
    Error (..),

    -- * Duration types.
    Seconds,
    Ticks,

    -- * Types.
    Timing (..),

    -- ** Prisms.
    timing,

    -- ** Parsers and serializers.
    decodeTiming,
    decode,
) where

-- Imports.
-- Base.
import Data.Ix (Ix)
import Data.Word (Word8, Word32)

-- Libraries.
import Optics.Core (Prism', prism')

-- non-Hackage libraries.
import Trisagion.Utils.Either ((:+:))
import Trisagion.Parser (Parser)
import Trisagion.Parsers.IStream (InputError)
import Trisagion.Parsers.Combinators (validate)
import qualified Trisagion.Parsers.Binary as Parsers (Binary (..))


{- | The t'Timing' @Error@ type. -}
newtype Error = Error Word8
    deriving stock (Eq, Ord, Bounded, Show)
    deriving newtype Enum


{- | The @Seconds@ duration type. -}
newtype Seconds = Seconds Word32
    deriving stock (Eq, Ord, Bounded, Ix)
    deriving newtype (Enum, Show)

{- | The @Ticks@ duration type. -}
newtype Ticks = Ticks Word32
    deriving stock (Eq, Ord, Bounded, Ix)
    deriving newtype (Enum, Show)


{- | The @Timing@ enumeration type for the timing mode of an opcode. -}
data Timing
    = Limited          {-# UNPACK #-} !Seconds
    | Instant
    | Equipped
    | DelayedLimited   {-# UNPACK #-} !Seconds
    | DelayedInstant   {-# UNPACK #-} !Seconds
    | DelayedEquipped  {-# UNPACK #-} !Seconds
    | DurationLimited  {-# UNPACK #-} !Ticks
    | DurationInstant  {-# UNPACK #-} !Ticks
    | DurationEquipped {-# UNPACK #-} !Ticks
    | Permanent
    | InstantLimited   {-# UNPACK #-} !Ticks
    deriving stock (Eq, Ord, Show)


{- | The prism for the t'Timing' type. -}
{-# INLINE timing #-}
timing :: Prism' (Word8, Word32) Timing
timing = prism' construct match
    where
        construct :: Timing -> (Word8, Word32)
        construct (Limited (Seconds n))         = (0, n)
        construct Instant                       = (1, 0)
        construct Equipped                      = (2, 0)
        construct (DelayedLimited (Seconds n))  = (3, n)
        construct (DelayedInstant (Seconds n))  = (4, n)
        construct (DelayedEquipped (Seconds n)) = (5, n)
        construct (DurationLimited (Ticks n))   = (6, n)
        construct (DurationInstant (Ticks n))   = (7, n)
        construct (DurationEquipped (Ticks n))  = (8, n)
        construct Permanent                     = (9, 0)
        construct (InstantLimited (Ticks n))    = (10, n)

        match :: (Word8, Word32) -> Maybe Timing
        match (0, n)  = Just $ Limited (Seconds n)
        match (1, _)  = Just $ Instant
        match (2, _)  = Just $ Equipped
        match (3, n)  = Just $ DelayedLimited (Seconds n)
        match (4, n)  = Just $ DelayedInstant (Seconds n)
        match (5, n)  = Just $ DelayedEquipped (Seconds n)
        match (6, n)  = Just $ DurationLimited (Ticks n)
        match (7, n)  = Just $ DurationInstant (Ticks n)
        match (8, n)  = Just $ DurationEquipped (Ticks n)
        match (9, _)  = Just $ Permanent
        match (10, n) = Just $ InstantLimited (Ticks n)
        match _       = Nothing


{- | Decode the first field of a t'Timing'. -}
{-# INLINE decodeTiming #-}
decodeTiming :: Parsers.Binary s => Parser s (InputError :+: Error) (Word32 -> Timing)
decodeTiming = validate v Parsers.word8
    where
        v :: Word8 -> Error :+: (Word32 -> Timing)
        v 0  = Right $ Limited . Seconds
        v 1  = Right $ const Instant
        v 2  = Right $ const Equipped
        v 3  = Right $ DelayedLimited . Seconds
        v 4  = Right $ DelayedInstant . Seconds
        v 5  = Right $ DelayedEquipped . Seconds
        v 6  = Right $ DurationLimited . Ticks
        v 7  = Right $ DurationInstant . Ticks
        v 8  = Right $ DurationEquipped . Ticks
        v 9  = Right $ const Permanent
        v 10 = Right $ InstantLimited . Ticks
        v n  = Left $ Error n

{- | Decode a t'Timing'.

Because of the way the timing information is layed out, it must be done in two steps:

@
do
    t <- decodeTiming   -- ^ Can throw here.
    ...
    timing <- decode t
@

For serializing, use the 'timing' prism and the standard serializers.
-}
{-# INLINE decode #-}
decode :: Parsers.Binary s => (Word32 -> Timing) -> Parser s InputError Timing
decode f = fmap f Parsers.word32Le
