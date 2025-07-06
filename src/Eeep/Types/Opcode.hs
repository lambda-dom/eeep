{- |
Module: Eeep.Types.Opcode

The @Opcode@ type.
-}


module Eeep.Types.Opcode (
    -- * Types.
) where


{- | The @Opcode@ type. -}
data Opcode = Opcode {
    optype      :: {-# UNPACK #-} !OpType,
    parameter1  :: {-# UNPACK #-} !Parameter,
    parameter2  :: {-# UNPACK #-} !Parameter,
    power       :: {-# UNPACK #-} !Power,
    target      :: {-# UNPACK #-} !Target,
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

