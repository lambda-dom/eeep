{- |
Module: Eeep.Types.Effect

The @Effect@ type.
-}

module Eeep.Types.Effect (
    -- * Types.
    Effect (..),
) where

-- Imports.
-- Package.
import Eeep.Types.Opcode.OpType (OpType)
import Eeep.Types.Opcode.Parameter (Parameter)
import Eeep.Types.Opcode.Power (Power)
import Eeep.Types.Opcode.Target (Target)
import Eeep.Types.Opcode.Timing (Timing)
import Eeep.Types.Opcode.Duration (Duration)
import Eeep.Types.Opcode.Probability (Probability)
import Eeep.Types.Opcode.ResistDispel (ResistDispel)
import Eeep.Types.Opcode.Resref (Resref)
import Eeep.Types.Opcode.DiceNumber (DiceNumber)
import Eeep.Types.Opcode.DiceSides (DiceSides)
import Eeep.Types.Opcode.SaveFlags (SaveFlags)
import Eeep.Types.Opcode.SaveBonus (SaveBonus)
import Eeep.Types.Effect.Projectile (Projectile)
import Eeep.Types.Effect.School (School)
import Eeep.Types.Effect.Sectype (Sectype)
import Eeep.Types.Opcode.Special (Special)


{- | The @Effect@ type. -}
data Effect = Effect {
    optype      :: {-# UNPACK #-} !OpType,
    parameter1  :: {-# UNPACK #-} !Parameter,
    parameter2  :: {-# UNPACK #-} !Parameter,
    parameter3  :: {-# UNPACK #-} !Parameter,
    parameter4  :: {-# UNPACK #-} !Parameter,
    power       :: {-# UNPACK #-} !Power,
    target      :: {-# UNPACK #-} !Target,
    timing      :: {-# UNPACK #-} !Timing,
    duration    :: {-# UNPACK #-} !Duration,
    probability :: {-# UNPACK #-} !Probability,
    dispel      :: {-# UNPACK #-} !ResistDispel,
    resource1   :: {-# UNPACK #-} !Resref,
    resource2   :: {-# UNPACK #-} !Resref,
    resource3   :: {-# UNPACK #-} !Resref,
    dicethrown  :: {-# UNPACK #-} !DiceNumber,
    dicesides   :: {-# UNPACK #-} !DiceSides,
    flags       :: {-# UNPACK #-} !SaveFlags,
    savebonus   :: {-# UNPACK #-} !SaveBonus,
    projectile  :: {-# UNPACK #-} !Projectile,
    school      :: {-# UNPACK #-} !School,
    sectype     :: {-# UNPACK #-} !Sectype,
    special     :: {-# UNPACK #-} !Special
    } deriving stock (Eq, Show)
