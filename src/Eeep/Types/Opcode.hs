{- |
Module: Eeep.Types.Opcode

The @Opcode@ type.
-}


module Eeep.Types.Opcode (
    -- * Error types.
    OpcodeError (..),

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
import Eeep.Types.Opcode.OpType (OpType, parseOpType16)
import Eeep.Types.Opcode.Parameter (Parameter, parseParameter)
import Eeep.Types.Opcode.Power (Power, parsePower8)
import Eeep.Types.Opcode.Target (Target, parseTarget8)
import Eeep.Types.Opcode.Timing (Timing, parseTiming8)
import Eeep.Types.Opcode.Duration (Duration, parseDuration)
import Eeep.Types.Opcode.Probability (Probability, parseProbability8)
import Eeep.Types.Opcode.Resref (Resref, parseResref)
import Eeep.Types.Opcode.ResistDispel (ResistDispel, parseResistDispel8)
import Eeep.Types.Opcode.DiceNumber (DiceNumber, parseDiceNumber)
import Eeep.Types.Opcode.DiceSides (DiceSides, parseDiceSides)
import Eeep.Types.Opcode.SaveFlags (SaveFlags, parseSaveFlags)
import Eeep.Types.Opcode.SaveBonus (SaveBonus, parseSaveBonus)
import Eeep.Types.Opcode.Special (Special, parseSpecial)


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
        optype      <- onError parseOpType16
        target      <- onError parseTarget8
        power       <- onError parsePower8
        parameter1  <- onError parseParameter
        parameter2  <- onError parseParameter
        timing      <- onError parseTiming8
        dispel      <- onError parseResistDispel8
        duration    <- onError parseDuration
        probability <- onError parseProbability8
        resource    <- onError parseResref
        dicenumber  <- onError parseDiceNumber
        dicesides   <- onError parseDiceSides
        saveflags   <- onError parseSaveFlags
        savebonus   <- onError parseSaveBonus
        special     <- onError parseSpecial
        pure Opcode {..}
    where
        onError :: (Typeable e, Eq e, Show e) => Parser s (ParseError e) a -> Parser s (ParseError OpcodeError) a
        onError = onParseError OpcodeError
