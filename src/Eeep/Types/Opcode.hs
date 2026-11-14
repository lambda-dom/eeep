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
import Data.Void (absurd)
import Data.Word (Word8, Word16, Word32)

-- Libraries.
import Optics.Core ((%), (^.), coercedTo, review, re, _1, _2)

-- non-Hackage libraries.
import Data.MonoFoldable (MonoFoldable)
import Trisagion.Utils.Either ((:+:))
import Trisagion.Parsers.IStream (InputError)
import Trisagion.Parser (Parser)
import Trisagion.Parsers.Split (skipN)
import qualified Trisagion.Parsers.Binary as Parsers (Binary (..))
import Trisagion.Serializer (Serializer)
import Trisagion.Serializers.Binary (nullN)
import qualified Trisagion.Serializers.Binary as Serializers (Binary (..))

-- Package.
import Eeep.Types.Shared.Dice (Dice)
import qualified Eeep.Types.Shared.Dice as Dice (dice)
import Eeep.Types.Shared.Resref (Resref)
import qualified Eeep.Types.Shared.Resref as Resref (decode, encode)
import Eeep.Types.Opcode.OpType (OpType)
import Eeep.Types.Opcode.OpType qualified as OpType (Error (..), decode, encode)
import Eeep.Types.Opcode.Target (Target)
import qualified Eeep.Types.Opcode.Target as Target (Error (..), decode, encode)
import Eeep.Types.Opcode.Power (Power)
import qualified Eeep.Types.Opcode.Power as Power (Error (..), decode, encode)
import Eeep.Types.Opcode.Timing (Timing)
import qualified Eeep.Types.Opcode.Timing as Timing (Error (..), decode, encode)
import Eeep.Types.Opcode.ResistDispel (ResistDispel)
import qualified Eeep.Types.Opcode.ResistDispel as ResistDispel (Error (..), decode, encode)
import Eeep.Types.Opcode.Probability (Probability)
import qualified Eeep.Types.Opcode.Probability as Probability (decode, error)
import Eeep.Types.Opcode.Flags (Flags)
import qualified Eeep.Types.Opcode.Flags as Flags (decode, encode)
import Eeep.Types.Opcode.SaveBonus (SaveBonus)
import qualified Eeep.Types.Opcode.SaveBonus as SaveBonus (Error (..), decode, encode)


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
    | SaveBonusError    {-# UNPACK #-} !Int32
    deriving stock (Eq, Ord, Generic, Show)


{- | The @Opcode@ type. -}
data Opcode = Opcode {
    optype       :: {-# UNPACK #-} !OpType,
    target       :: {-# UNPACK #-} !Target,
    parameter1   :: {-# UNPACK #-} !Word32,
    parameter2   :: {-# UNPACK #-} !Word32,
    power        :: {-# UNPACK #-} !Power,
    timing       :: {-# UNPACK #-} !Timing,
    duration     :: {-# UNPACK #-} !Word32,
    resistdispel :: {-# UNPACK #-} !ResistDispel,
    probability  :: {-# UNPACK #-} !Probability,
    resref       :: {-# UNPACK #-} !Resref,
    dice         :: {-# UNPACK #-} !Dice,
    flags        :: {-# UNPACK #-} !Flags,
    savebonus    :: {-# UNPACK #-} !SaveBonus,
    special      :: {-# UNPACK #-} !Word32
} deriving stock (Eq, Show, Generic)


{- | Parser for the @t'Opcode'@ type. -}
{-# INLINEABLE decode #-}
decode :: (Parsers.Binary b s, MonoFoldable Word8 b) => Parser s (InputError :+: Error) Opcode
decode = do
    optype       <- first (fmap (review $ #_OpTypeError % coercedTo)) OpType.decode
    target       <- first (fmap (review $ #_TargetError % coercedTo)) Target.decode
    power        <- first (fmap (review $ #_PowerError % coercedTo)) Power.decode
    parameter1   <- first Left Parsers.word32Le
    parameter2   <- first Left Parsers.word32Le
    timing       <- first (fmap (review $ #_TimingError % coercedTo)) Timing.decode
    resistdispel <- first (fmap (review $ #_ResistDispelError % coercedTo)) ResistDispel.decode
    duration     <- first Left Parsers.word32Le
    probability  <- first (fmap (review $ #_ProbabilityError % re Probability.error)) Probability.decode
    resref       <- first Left Resref.decode
    number       <- first Left Parsers.word8
    _            <- first absurd $ skipN 3
    sides        <- first Left Parsers.word8
    _            <- first absurd $ skipN 3
    let dice = review Dice.dice (number, sides)
    flags        <- first Left Flags.decode
    savebonus    <- first (fmap (review $ #_SaveBonusError % coercedTo)) SaveBonus.decode
    special      <- first Left Parsers.word32Le
    pure Opcode {..}

{- | Serializer for the t'Opcode' type. -}
{-# INLINEABLE encode #-}
encode :: Serializers.Binary s => Serializer s Opcode
encode
    =  contramap (^. #optype) OpType.encode
    <> contramap (^. #target) Target.encode
    <> contramap (^. #power) Power.encode
    <> contramap (^. #parameter1) Serializers.word32Le
    <> contramap (^. #parameter2) Serializers.word32Le
    <> contramap (^. #timing) Timing.encode
    <> contramap (^. #resistdispel) ResistDispel.encode
    <> contramap (^. #duration) Serializers.word32Le
    <> contramap (^. #resref) Resref.encode
    <> contramap (^. #dice % Dice.dice % _1) Serializers.word8
    <> nullN 3
    <> contramap (^. #dice % Dice.dice % _2) Serializers.word8
    <> nullN 3
    <> contramap (^. #flags) Flags.encode
    <> contramap (^. #savebonus) SaveBonus.encode
    <> contramap (^. #special) Serializers.word32Le
