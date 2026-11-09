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

    -- ** Parsers and serializers.
    decode,
    encode,
) where

-- Imports.
-- Base.
import GHC.Generics (Generic)
import Data.Bifunctor (Bifunctor (..))
import Data.Functor.Contravariant (Contravariant (..))
import Data.Int (Int32)
import Data.Word (Word8, Word16)

-- Libraries.
import Data.ByteString.Short (ShortByteString)
import Optics.Core ((%), (^.), coercedTo, review, re)

-- non-Hackage libraries.
import Mono.Typeclasses.MonoFoldable (MonoFoldable)
import Trisagion.Utils.Either ((:+:))
import qualified Trisagion.Typeclasses.Parsers.Binary as Parsers (Binary)
import Trisagion.Parsers.Source (InputError)
import Trisagion.Parser (Parser)
import qualified Trisagion.Typeclasses.Serializers.Binary as Serializers (Binary (..))
import Trisagion.Serializer (Serializer)

-- Package.
import Eeep.Types.Shared.Dice (Dice)
import qualified Eeep.Types.Shared.Dice as Dice (decode, encode)
import Eeep.Types.Shared.Resref (Resref)
import qualified Eeep.Types.Shared.Resref as Resref (Error (..), decode, encode)
import Eeep.Types.Opcode.OpType (OpType)
import Eeep.Types.Opcode.OpType qualified as OpType (Error (..), decode, encode)
import Eeep.Types.Opcode.Target (Target)
import qualified Eeep.Types.Opcode.Target as Target (Error (..), decode, encode)
import Eeep.Types.Opcode.Parameter (Parameter)
import qualified Eeep.Types.Opcode.Parameter as Parameter (decode, encode)
import Eeep.Types.Opcode.Power (Power)
import qualified Eeep.Types.Opcode.Power as Power (Error (..), decode, encode)
import Eeep.Types.Opcode.Timing (Timing)
import qualified Eeep.Types.Opcode.Timing as Timing (Error (..), decode, encode)
import Eeep.Types.Opcode.Duration (Duration)
import qualified Eeep.Types.Opcode.Duration as Duration (decode, encode)
import Eeep.Types.Opcode.ResistDispel (ResistDispel)
import qualified Eeep.Types.Opcode.ResistDispel as ResistDispel (Error (..), decode, encode)
import Eeep.Types.Opcode.Probability (Probability)
import qualified Eeep.Types.Opcode.Probability as Probability (decode, error)
import Eeep.Types.Opcode.Flags (Flags)
import qualified Eeep.Types.Opcode.Flags as Flags (decode, encode)
import Eeep.Types.Opcode.SaveBonus (SaveBonus)
import qualified Eeep.Types.Opcode.SaveBonus as SaveBonus (Error (..), decode, encode)
import Eeep.Types.Opcode.Special (Special)
import qualified Eeep.Types.Opcode.Special as Special (decode, encode)


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
    optype       :: {-# UNPACK #-} !OpType,
    target       :: {-# UNPACK #-} !Target,
    parameter1   :: {-# UNPACK #-} !Parameter,
    parameter2   :: {-# UNPACK #-} !Parameter,
    power        :: {-# UNPACK #-} !Power,
    timing       :: {-# UNPACK #-} !Timing,
    duration     :: {-# UNPACK #-} !Duration,
    resistdispel :: {-# UNPACK #-} !ResistDispel,
    probability  :: {-# UNPACK #-} !Probability,
    resref       :: {-# UNPACK #-} !Resref,
    dice         :: {-# UNPACK #-} !Dice,
    flags        :: {-# UNPACK #-} !Flags,
    savebonus    :: {-# UNPACK #-} !SaveBonus,
    special      :: {-# UNPACK #-} !Special
} deriving stock (Eq, Show, Generic)


{- | Parser for the @t'Opcode'@ type. -}
{-# INLINEABLE decode #-}
decode :: (Parsers.Binary b s, MonoFoldable Word8 b) => Parser s (Error :+: InputError) Opcode
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
    flags        <- first Right Flags.decode
    savebonus    <- first (first (review $ #_SaveBonusError % coercedTo)) SaveBonus.decode
    special      <- first Right Special.decode
    pure Opcode {..}

{- | Serializer for the t'Opcode' type. -}
{-# INLINEABLE encode #-}
encode :: Serializers.Binary b s => Serializer s Opcode
encode
    =  contramap (^. #optype) OpType.encode
    <> contramap (^. #target) Target.encode
    <> contramap (^. #power) Power.encode
    <> contramap (^. #parameter1) Parameter.encode
    <> contramap (^. #parameter2) Parameter.encode
    <> contramap (^. #timing) Timing.encode
    <> contramap (^. #resistdispel) ResistDispel.encode
    <> contramap (^. #duration) Duration.encode
    <> contramap (^. #resref) Resref.encode
    <> contramap (^. #dice) Dice.encode
    <> contramap (^. #flags) Flags.encode
    <> contramap (^. #savebonus) SaveBonus.encode
    <> contramap (^. #special) Special.encode
