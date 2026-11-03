{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module: Eeep.Types.Opcode

The @Opcode@ type.
-}

module Eeep.Types.Opcode (
    -- * Error types.
    Error (..),

    -- * Types.
    Opcode,

    -- ** Parsers.
    decode,
) where

-- Imports.
-- Base.
import GHC.Generics (Generic)
import Data.Bifunctor (Bifunctor (..))
import Data.Int (Int32)
import Data.Word (Word8, Word16)

-- Libraries.
import Data.ByteString.Short (ShortByteString)
import Optics.Core ((%), coercedTo, review, re)

-- non-Hackage libraries.
import Mono.Typeclasses.MonoFoldable (MonoFoldable)
import Trisagion.Utils.Either ((:+:))
import Trisagion.Parsers.Source (InputError)
import Trisagion.Parser (Parser)
import Trisagion.Typeclasses.Parsers.Binary (Binary)

-- Package.
import Eeep.Types.Opcode.OpType (OpType)
import Eeep.Types.Opcode.OpType qualified as OpType (Error (..), decode)
import Eeep.Types.Opcode.Target (Target)
import qualified Eeep.Types.Opcode.Target as Target (Error (..), decode)
import Eeep.Types.Opcode.Parameter (Parameter)
import qualified Eeep.Types.Opcode.Parameter as Parameter (decode)
import Eeep.Types.Opcode.Power (Power)
import qualified Eeep.Types.Opcode.Power as Power (Error (..), decode)
import Eeep.Types.Opcode.Timing (Timing)
import qualified Eeep.Types.Opcode.Timing as Timing (Error (..), decode)
import Eeep.Types.Opcode.Duration (Duration)
import qualified Eeep.Types.Opcode.Duration as Duration (decode)
import Eeep.Types.Opcode.ResistDispel (ResistDispel)
import qualified Eeep.Types.Opcode.ResistDispel as ResistDispel (Error (..), decode)
import Eeep.Types.Opcode.Probability (Probability)
import qualified Eeep.Types.Opcode.Probability as Probability (decode, error)
import Eeep.Types.Opcode.Resref (Resref)
import qualified Eeep.Types.Opcode.Resref as Resref (Error (..), decode)
import Eeep.Types.Opcode.Dice (Dice)
import qualified Eeep.Types.Opcode.Dice as Dice (decode)
import Eeep.Types.Opcode.SaveFlags (SaveFlags)
import qualified Eeep.Types.Opcode.SaveFlags as SaveFlags (decode)
import Eeep.Types.Opcode.SaveBonus (SaveBonus)
import qualified Eeep.Types.Opcode.SaveBonus as SaveBonus (Error (..), decode)
import Eeep.Types.Opcode.Special (Special)
import qualified Eeep.Types.Opcode.Special as Special (decode)


{- | The t'Opcode' @Error@ type.

note(s):

    * To be imported qualified.
-}
data Error
    = OpTypeError       {-# UNPACK #-} !Word16
    | TargetError       {-# UNPACK #-} !Word8
    | PowerError        {-# UNPACK #-} !Word8
    | TimingError       {-# UNPACK #-} !Word8
    | ResistDispelError {-# UNPACK #-} !Word8
    | ProbabilityError  {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8
    | ResrefError       {-# UNPACK #-} !ShortByteString
    | SaveBonusError    {-# UNPACK #-} !Int32
    deriving stock (Eq, Ord, Generic, Show)


{- | The @Opcode@ type. -}
data Opcode = Opcode {
    optype       :: !OpType,
    target       :: !Target,
    parameter1   :: !Parameter,
    parameter2   :: !Parameter,
    power        :: !Power,
    timing       :: !Timing,
    duration     :: !Duration,
    resistdispel :: !ResistDispel,
    probability  :: !Probability,
    resref       :: !Resref,
    dice         :: !Dice,
    saveflags    :: !SaveFlags,
    savebonus    :: !SaveBonus,
    special      :: !Special
} deriving stock (Eq, Generic, Show)


{- | Parser for the @t'Opcode'@ type. -}
{-# INLINEABLE decode #-}
decode :: (Binary b s, MonoFoldable Word8 b) => Parser s (Error :+: InputError) Opcode
decode = do
    optype       <- first (first (review $ #_OpTypeError % coercedTo)) OpType.decode
    target       <- first (first (review $ #_TargetError % coercedTo)) Target.decode
    power        <- first (first (review $ #_PowerError % coercedTo)) Power.decode
    parameter1   <- first Right Parameter.decode
    parameter2   <- first Right Parameter.decode
    timing       <- first (first (review $ #_TimingError % coercedTo)) Timing.decode
    resistdispel <- first (first (review $ #_ResistDispelError % coercedTo)) ResistDispel.decode
    duration     <- first Right Duration.decode
    probability  <- first (first (review $ #_ProbabilityError % re Probability.error)) Probability.decode
    resref       <- first (first (review $ #_ResrefError % coercedTo)) Resref.decode
    dice         <- first Right Dice.decode
    saveflags    <- first Right SaveFlags.decode
    savebonus    <- first (first (review $ #_SaveBonusError % coercedTo)) SaveBonus.decode
    special      <- first Right Special.decode
    pure Opcode {..}
