{-# LANGUAGE NoFieldSelectors #-}

{- |
Module: Eeep.Types.Opcode

The @Opcode@ type.
-}

module Eeep.Types.Opcode (
    -- * Types.
    Opcode,
) where

-- Imports.
-- Base.
import GHC.Generics (Generic)

-- Package.
import Eeep.Types.Opcode.OpType (OpType)
import Eeep.Types.Opcode.Target (Target)
import Eeep.Types.Opcode.Parameter (Parameter)
import Eeep.Types.Opcode.Power (Power)
import Eeep.Types.Opcode.Timing (Timing)
import Eeep.Types.Opcode.Duration (Duration)
import Eeep.Types.Opcode.ResistDispel (ResistDispel)
import Eeep.Types.Opcode.Probability (Probability)
import Eeep.Types.Opcode.Resref (Resref)
import Eeep.Types.Opcode.Dice (Dice)
import Eeep.Types.Opcode.SaveFlags (SaveFlags)
import Eeep.Types.Opcode.SaveBonus (SaveBonus)
import Eeep.Types.Opcode.Special (Special)


{- | The @Opcode@ type. -}
data Opcode = Opcode {
    opType       :: !OpType,
    target       :: !Target,
    parameter1   :: !Parameter,
    parameter2   :: !Parameter,
    power        :: !Power,
    timing       :: !Timing,
    duration     :: !Duration,
    resistDispel :: !ResistDispel,
    probability  :: !Probability,
    resref       :: !Resref,
    dice         :: !Dice,
    saveflags    :: !SaveFlags,
    savebonus    :: !SaveBonus,
    special      :: !Special
} deriving stock (Eq, Generic, Show)
