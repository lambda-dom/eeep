{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedLabels #-}

{- |
Module: Eeep.Types.Opcode

The @Opcode@ type.
-}


module Eeep.Types.Opcode (
    -- * Error types.
    OpcodeError,

    -- * Types.
    Opcode,
) where

-- Imports.
-- Base.
import GHC.Generics (Generic)
import Data.Functor.Contravariant (Contravariant (..))
import Data.Typeable (Typeable)
import Data.Word (Word8)

-- Libraries.
import Optics.Core ((^.))

-- non-Hackage libraries.
import Mono.Typeclasses.MonoFunctor (ElementOf)
import Mono.Typeclasses.MonoFoldable (MonoFoldable)
import Trisagion.Types.ParseError (ParseError)
import Trisagion.Typeclasses.HasOffset (HasOffset)
import Trisagion.Typeclasses.Splittable (Splittable (PrefixOf))
import Trisagion.Typeclasses.Binary (Binary)
import Trisagion.Parser (Parser)
import Trisagion.Parsers.ParseError (capture, onParseError)
import Trisagion.Serializer (Serializer)

-- Package.
import Eeep.Typeclasses.Binary (Reader (..), Writer (..))
import Eeep.Types.Opcode.OpType (OpType, decodeOpType16, encodeOpType16)
import Eeep.Types.Opcode.Power (Power, decodePower8, encodePower8)
import Eeep.Types.Opcode.Target (Target, decodeTarget8, encodeTarget8)
import Eeep.Types.Opcode.Parameter (Parameter, decodeParameter, encodeParameter)
import Eeep.Types.Opcode.Timing (Timing, decodeTiming8, encodeTiming8)
import Eeep.Types.Opcode.Duration (Duration, decodeDuration, encodeDuration)
import Eeep.Types.Opcode.ResistDispel (ResistDispel, decodeResistDispel8, encodeResistDispel8)
import Eeep.Types.Opcode.Probability (Probability, decodeProbability8, encodeProbability8)
import Eeep.Types.Opcode.Resref (Resref, decodeResref, encodeResref)
import Eeep.Types.Opcode.DiceNumber (DiceNumber, decodeDiceNumber, encodeDiceNumber)
import Eeep.Types.Opcode.DiceSides (DiceSides, decodeDiceSides, encodeDiceSides)
import Eeep.Types.Opcode.SaveFlags (SaveFlags, decodeSaveFlags, encodeSaveFlags)
import Eeep.Types.Opcode.SaveBonus (SaveBonus, decodeSaveBonus, encodeSaveBonus)
import Eeep.Types.Opcode.Special (Special, decodeSpecial, encodeSpecial)


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
    } deriving stock (Eq, Show, Generic)


-- Instances.
instance Reader (ParseError OpcodeError) Opcode where
    parser = decodeOpcode

instance Writer Opcode where
    serializer = encodeOpcode


{- | Decode an t'Opcode'. -}
decodeOpcode
    :: forall s . (HasOffset s, Splittable s, MonoFoldable (PrefixOf s), ElementOf s ~ Word8, ElementOf (PrefixOf s) ~ Word8)
    => Parser s (ParseError OpcodeError) Opcode
decodeOpcode = capture $ do
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


{- | Encode an t'Opcode'. -}
encodeOpcode :: Binary m => Serializer m Opcode
encodeOpcode
    =  contramap (^. #optype) encodeOpType16
    <> contramap (^. #target) encodeTarget8
    <> contramap (^. #power) encodePower8
    <> contramap (^. #parameter1) encodeParameter
    <> contramap (^. #parameter2) encodeParameter
    <> contramap (^. #timing) encodeTiming8
    <> contramap (^. #dispel) encodeResistDispel8
    <> contramap (^. #duration) encodeDuration
    <> contramap (^. #probability) encodeProbability8
    <> contramap (^. #resource) encodeResref
    <> contramap (^. #dicenumber) encodeDiceNumber
    <> contramap (^. #dicesides) encodeDiceSides
    <> contramap (^. #saveflags) encodeSaveFlags
    <> contramap (^. #savebonus) encodeSaveBonus
    <> contramap (^. #special) encodeSpecial
