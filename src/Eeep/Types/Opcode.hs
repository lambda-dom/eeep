{- |
Module: Eeep.Types.Opcode

The @Opcode@ type.
-}


module Eeep.Types.Opcode (
    -- * Error types.
    OpcodeError,

    -- * Types.
    Opcode (..),

    -- * Parsers.
    parseOpcode,
) where

-- Imports.
-- Base.
import Data.Typeable (Typeable)
import Data.Word (Word8)

-- non-Hackage libraries.
import Mono.Typeclasses.MonoFunctor (ElementOf)
import Mono.Typeclasses.MonoFoldable (MonoFoldable)
import Trisagion.Types.ParseError (ParseError)
import Trisagion.Typeclasses.HasOffset (HasOffset)
import Trisagion.Typeclasses.Splittable (Splittable (PrefixOf))
import Trisagion.Parser (Parser)
import Trisagion.Parsers.ParseError (capture, onParseError)

-- Package.
import Eeep.Types.Opcode.OpType (OpType, decodeOpType16)
import Eeep.Types.Opcode.Power (Power, decodePower8)
import Eeep.Types.Opcode.Target (Target, decodeTarget8)
import Eeep.Types.Opcode.Parameter (Parameter, decodeParameter)
import Eeep.Types.Opcode.Timing (Timing, decodeTiming8)
import Eeep.Types.Opcode.Duration (Duration, decodeDuration)
import Eeep.Types.Opcode.ResistDispel (ResistDispel, decodeResistDispel8)
import Eeep.Types.Opcode.Probability (Probability, decodeProbability8)
import Eeep.Types.Opcode.Resref (Resref, decodeResref)
import Eeep.Types.Opcode.DiceNumber (DiceNumber, decodeDiceNumber)
import Eeep.Types.Opcode.DiceSides (DiceSides, decodeDiceSides)
import Eeep.Types.Opcode.SaveFlags (SaveFlags, decodeSaveFlags)
import Eeep.Types.Opcode.SaveBonus (SaveBonus, decodeSaveBonus)
import Eeep.Types.Opcode.Special (Special, decodeSpecial)


{- | The t'OpcodeError' type. -}
data OpcodeError = OpcodeError
    deriving stock (Eq, Show)


{- | The @Opcode@ type. -}
data Opcode = Opcode {
    optype      :: {-# UNPACK #-} !OpType,
    target      :: {-# UNPACK #-} !Target,
    power       :: {-# UNPACK #-} !Power,
    parameter1  :: {-# UNPACK #-} !Parameter,
    parameter2  :: {-# UNPACK #-} !Parameter,
    timing      :: {-# UNPACK #-} !Timing,
    duration    :: {-# UNPACK #-} !Duration,
    probability :: {-# UNPACK #-} !Probability,
    dispel      :: {-# UNPACK #-} !ResistDispel,
    resource    :: {-# UNPACK #-} !Resref,
    dicenumber  :: {-# UNPACK #-} !DiceNumber,
    dicesides   :: {-# UNPACK #-} !DiceSides,
    saveflags   :: {-# UNPACK #-} !SaveFlags,
    savebonus   :: {-# UNPACK #-} !SaveBonus,
    special     :: {-# UNPACK #-} !Special
    } deriving stock (Eq, Show)


{- | Parse an t'Effect'. -}
parseOpcode
    :: forall s . (HasOffset s, Splittable s, MonoFoldable (PrefixOf s), ElementOf s ~ Word8, ElementOf (PrefixOf s) ~ Word8)
    => Parser s (ParseError OpcodeError) Opcode
parseOpcode = capture $ do
        optype      <- onError decodeOpType16
        target      <- onError decodeTarget8
        power       <- onError decodePower8
        parameter1  <- onError decodeParameter
        parameter2  <- onError decodeParameter
        timing      <- onError decodeTiming8
        dispel      <- onError decodeResistDispel8
        duration    <- onError decodeDuration
        probability <- onError decodeProbability8
        resource    <- onError decodeResref
        dicenumber  <- onError decodeDiceNumber
        dicesides   <- onError decodeDiceSides
        saveflags   <- onError decodeSaveFlags
        savebonus   <- onError decodeSaveBonus
        special     <- onError decodeSpecial
        pure Opcode {..}
    where
        onError :: (Typeable e, Eq e, Show e) => Parser s (ParseError e) a -> Parser s (ParseError OpcodeError) a
        onError = onParseError OpcodeError
