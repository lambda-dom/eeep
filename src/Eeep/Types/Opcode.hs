{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module: Eeep.Types.Opcode

The @Opcode@ type.
-}


module Eeep.Types.Opcode (
    -- * Error types.
    OpcodeError (..),

    -- * Types.
    Opcode (..),
) where

-- Imports.
-- Base.
import Data.Bifunctor (Bifunctor (..))
import Data.Int (Int32)
import Data.Void (absurd)
import Data.Word (Word8, Word16)

-- non-Hackage libraries.
import Mono.Typeclasses.MonoFunctor (ElementOf)
import Mono.Typeclasses.MonoFoldable (MonoFoldable)
import Trisagion.Types.ParseError (ParseError)
import Trisagion.Typeclasses.HasOffset (HasOffset)
import Trisagion.Typeclasses.Splittable (Splittable (PrefixOf))
import Trisagion.Parser (Parser)
import Trisagion.Parsers.ParseError (capture)

-- Package.
import Eeep.Typeclasses.Binary (Reader (..))
import Eeep.Types.Opcode.OpType (OpType, OpTypeError)
import Eeep.Types.Opcode.Parameter (Parameter (..))
import Eeep.Types.Opcode.Power (Power, PowerError)
import Eeep.Types.Opcode.Target (Target, TargetError)
import Eeep.Types.Opcode.Timing (Timing, TimingError)
import Eeep.Types.Opcode.Duration (Duration)
import Eeep.Types.Opcode.Probability (Probability, ProbabilityError)
import Eeep.Types.Opcode.Resref (Resref)
import Eeep.Types.Opcode.ResistDispel (ResistDispel, ResistDispelError)
import Eeep.Types.Opcode.DiceNumber (DiceNumber)
import Eeep.Types.Opcode.DiceSides (DiceSides)
import Eeep.Types.Opcode.SaveFlags (SaveFlags)
import Eeep.Types.Opcode.SaveBonus (SaveBonus, SaveBonusError)
import Eeep.Types.Opcode.Special (Special)
import qualified Eeep.Types.Opcode.OpType as OpType (OpTypeError (..))
import qualified Eeep.Types.Opcode.Target as Target (TargetError (..))
import qualified Eeep.Types.Opcode.Power as Power (PowerError (..))
import qualified Eeep.Types.Opcode.Timing as Timing (TimingError (..))
import qualified Eeep.Types.Opcode.ResistDispel as ResistDispel (ResistDispelError (..))
import qualified Eeep.Types.Opcode.Probability as Probability (ProbabilityError (..))
import qualified Eeep.Types.Opcode.SaveBonus as SaveBonus (SaveBonusError (..))


{- | The t'OpcodeError' type. -}
data OpcodeError
    = OpTypeError !Word16
    | TargetError !Word8
    | PowerError !Word8
    | TimingError !Word8
    | ResistDispelError !Word8
    | ProbabilityError !Word8 !Word8
    | SaveBonusError !Int32
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



instance (HasOffset s, Splittable s, MonoFoldable (PrefixOf s), ElementOf (PrefixOf s) ~ Word8, ElementOf s ~ Word8)
    => Reader s OpcodeError Opcode where
    parser :: Parser s (ParseError OpcodeError) Opcode
    parser = capture $ do
            optype <- first (fmap fromOpTypeError) parser
            target <- first (fmap fromTargetError) parser
            power <- first (fmap fromPowerError) parser
            parameter1 <- first (fmap absurd) parser
            parameter2 <- first (fmap absurd) parser
            timing <- first (fmap fromTimingError) parser
            dispel <- first (fmap fromResistDispelError) parser
            duration <- first (fmap absurd) parser
            probability <- first (fmap fromProbabilityError) parser
            resource <- first (fmap absurd) parser
            dicenumber <- first (fmap absurd) parser
            dicesides <- first (fmap absurd) parser
            saveflags <- first (fmap absurd) parser
            savebonus<- first (fmap fromSaveBonusError) parser
            special <- first (fmap absurd) parser
            pure Opcode {..}
        where
            fromOpTypeError :: OpTypeError -> OpcodeError
            fromOpTypeError (OpType.OpTypeError n) = OpTypeError n

            fromTargetError :: TargetError -> OpcodeError
            fromTargetError (Target.TargetError n) = TargetError n

            fromPowerError :: PowerError -> OpcodeError
            fromPowerError (Power.PowerError n) = PowerError n

            fromTimingError :: TimingError -> OpcodeError
            fromTimingError (Timing.TimingError n) = TimingError n

            fromResistDispelError :: ResistDispelError -> OpcodeError
            fromResistDispelError (ResistDispel.ResistDispelError n) = ResistDispelError n

            fromProbabilityError :: ProbabilityError -> OpcodeError
            fromProbabilityError (Probability.ProbabilityError n m) = ProbabilityError n m

            fromSaveBonusError :: SaveBonusError -> OpcodeError
            fromSaveBonusError (SaveBonus.SaveBonusError n) = SaveBonusError n
