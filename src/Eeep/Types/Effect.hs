{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module: Eeep.Types.Effect

The @Effect@ type.
-}

module Eeep.Types.Effect (
    -- * Error types.
    Error (..),

    -- * Types.
    Effect (..),

    -- ** Parsers and serializers.
    signature,
    decodeBody,
    decode,
    encodeBody,
    encode,
) where

-- Imports.
-- Base.
import GHC.Generics (Generic)
import Data.Bifunctor (Bifunctor (..))
import Data.Char (ord)
import Data.Functor.Contravariant (Contravariant (..))
import Data.Int (Int32)
import Data.Void (absurd)
import Data.Word (Word8, Word16, Word32)

-- Libraries.
import Optics.Core ((%), (^.), review, coercedTo, preview, view, _1, _2)

-- non-Hackage libraries.
import Data.MonoFoldable (MonoFoldable (..))
import Trisagion.Utils.Either ((:+:))
import qualified Trisagion.Parsers.Binary as Parsers (Binary (..))
import Trisagion.Parser (Parser, throw)
import Trisagion.Parsers.IStream (InputError, ValidationError (..), matchFoldable)
import Trisagion.Parsers.Split (skipN, skipExact)
import qualified Trisagion.Serializers.Binary as Serializers (Binary (..))
import Trisagion.Serializer (Serializer)
import Trisagion.Serializers.Combinators (value)
import Trisagion.Serializers.Binary (nullN, char8s)

-- Package.
import Eeep.Types.Shared.Dice (Dice)
import qualified Eeep.Types.Shared.Dice as Dice (dice)
import Eeep.Types.Shared.Resref (Resref)
import qualified Eeep.Types.Shared.Resref as Resref (decode, encode)
import Eeep.Types.Shared.Projectile (Projectile)
import qualified Eeep.Types.Shared.Projectile as Projectile (decode, encode)
import Eeep.Types.Shared.School (School)
import qualified Eeep.Types.Shared.School as School (decode, encode)
import Eeep.Types.Shared.Sectype (Sectype)
import qualified Eeep.Types.Shared.Sectype as Sectype (decode, encode)
import Eeep.Types.Opcode.OpType (OpType)
import qualified Eeep.Types.Opcode.OpType as OpType (Error (..), decode, encode)
import Eeep.Types.Opcode.Power (Power)
import qualified Eeep.Types.Opcode.Power as Power (Error (..), decode, encode)
import Eeep.Types.Opcode.Target (Target)
import qualified Eeep.Types.Opcode.Target as Target (Error (..), decode, encode)
import Eeep.Types.Opcode.Timing (Timing)
import qualified Eeep.Types.Opcode.Timing as Timing (Error (..), decode, encode)
import Eeep.Types.Opcode.Probability (Probability)
import qualified Eeep.Types.Opcode.Probability as Probability (probability, upper, lower)
import Eeep.Types.Opcode.ResistDispel (ResistDispel)
import qualified Eeep.Types.Opcode.ResistDispel as ResistDispel (Error (..), decode, encode)
import Eeep.Types.Opcode.Flags (Flags)
import qualified Eeep.Types.Opcode.Flags as Flags (decode, encode)
import Eeep.Types.Opcode.SaveBonus (SaveBonus)
import qualified Eeep.Types.Opcode.SaveBonus as SaveBonus (Error (..), decode, encode)


{- | The t'Effect' @Error@ type.

note(s):

    * To be imported qualified.
-}
data Error
    = SignatureError    {-# UNPACK #-} !Word8
    | OpTypeError       {-# UNPACK #-} !Word16
    | TargetError       {-# UNPACK #-} !Word8
    | PowerError        {-# UNPACK #-} !Word8
    | TimingError       {-# UNPACK #-} !Word8
    | ResistDispelError {-# UNPACK #-} !Word8
    | ProbabilityError  {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8
    | SaveBonusError    {-# UNPACK #-} !Int32
    deriving stock (Eq, Ord, Generic, Show)


{- | The @Effect@ type. -}
data Effect = Effect {
    optype       :: {-# UNPACK #-} !OpType,
    parameter1   :: {-# UNPACK #-} !Word32,
    parameter2   :: {-# UNPACK #-} !Word32,
    parameter3   :: {-# UNPACK #-} !Word32,
    parameter4   :: {-# UNPACK #-} !Word32,
    power        :: {-# UNPACK #-} !Power,
    target       :: {-# UNPACK #-} !Target,
    timing       :: {-# UNPACK #-} !Timing,
    duration     :: {-# UNPACK #-} !Word32,
    probability  :: {-# UNPACK #-} !Probability,
    resistdispel :: {-# UNPACK #-} !ResistDispel,
    resref1      :: {-# UNPACK #-} !Resref,
    resref2      :: {-# UNPACK #-} !Resref,
    dice         :: {-# UNPACK #-} !Dice,
    flags        :: {-# UNPACK #-} !Flags,
    savebonus    :: {-# UNPACK #-} !SaveBonus,
    projectile   :: {-# UNPACK #-} !Projectile,
    school       :: {-# UNPACK #-} !School,
    sectype      :: {-# UNPACK #-} !Sectype,
    special      :: {-# UNPACK #-} !Word32
} deriving stock (Eq, Show, Generic)


{- | Parser for the @t'Effect'@ signature. -}
{-# INLINE signature #-}
signature
    :: forall b s . (Parsers.Binary b s)
    => Parser s (InputError :+: Error) ()
signature =
    first
        (fmap (\ (ValidationError n) -> SignatureError n))
        (matchFoldable (fmap (fromIntegral @Int @Word8 . ord) "EFF V2.0"))


-- | Parser for t'Probability'. Not exported.
{-# INLINE probParser #-}
probParser :: Parsers.Binary b s => Parser s (InputError :+: Error) Probability
probParser = do
    upper <- first Left Parsers.word8
    _     <- first absurd $ skipN 1
    lower <- first Left Parsers.word8
    _     <- first absurd $ skipN 1
    case preview Probability.probability (lower, upper) of
        Just x  -> pure x
        Nothing -> throw . Right $ ProbabilityError lower upper


{- | Parser for an embedded @t'Effect'@. -}
{-# INLINEABLE decodeBody #-}
decodeBody :: (Parsers.Binary b s, MonoFoldable Word8 b) => Parser s (InputError :+: Error) Effect
decodeBody = do
    optype       <- first (fmap (review $ #_OpTypeError % coercedTo)) OpType.decode
    _            <- first absurd $ skipN 2
    target       <- first (fmap (review $ #_TargetError % coercedTo)) Target.decode
    _            <- first absurd $ skipN 3
    power        <- first (fmap (review $ #_PowerError % coercedTo)) Power.decode
    _            <- first absurd $ skipN 3
    parameter1   <- first Left Parsers.word32Le
    parameter2   <- first Left Parsers.word32Le
    timing       <- first (fmap (review $ #_TimingError % coercedTo)) Timing.decode
    _            <- first absurd $ skipN 3
    duration     <- first Left Parsers.word32Le
    probability  <- probParser
    resref1      <- first Left Resref.decode
    number       <- first Left Parsers.word8
    _            <- first absurd $ skipN 3
    sides        <- first Left Parsers.word8
    _            <- first absurd $ skipN 3
    let dice = review Dice.dice (number, sides)
    flags        <- first Left Flags.decode
    savebonus    <- first (fmap (review $ #_SaveBonusError % coercedTo)) SaveBonus.decode
    special      <- first Left Parsers.word32Le
    school       <- first Left School.decode
    _            <- first absurd $ skipN 15
    resistdispel <- first (fmap (review $ #_ResistDispelError % coercedTo)) ResistDispel.decode
    _            <- first absurd $ skipN 3
    parameter3   <- first Left Parsers.word32Le
    parameter4   <- first Left Parsers.word32Le
    _            <- first absurd $ skipN 8
    resref2      <- first Left Resref.decode
    _            <- first absurd $ skipN 40
    projectile   <- first Left Projectile.decode
    _            <- first absurd $ skipN 44
    sectype      <- first Left Sectype.decode
    _            <- first Left $ skipExact 63
    pure Effect {..}

{- | Parser for a file @t'Effect'@. -}
{-# INLINE decode #-}
decode :: (Parsers.Binary b s, MonoFoldable Word8 b) => Parser s (InputError :+: Error) Effect
decode = do
    _ <- signature
    _ <- first absurd $ skipN 8
    decodeBody
    
{- | Serializer for an embedded t'Effect'. -}
{-# INLINEABLE encodeBody #-}
encodeBody :: Serializers.Binary s => Serializer s Effect
encodeBody
    =  contramap (^. #optype) OpType.encode
    <> nullN 2
    <> contramap (^. #target) Target.encode
    <> nullN 3
    <> contramap (^. #power) Power.encode
    <> nullN 3
    <> contramap (^. #parameter1) Serializers.word32Le
    <> contramap (^. #parameter2) Serializers.word32Le
    <> contramap (^. #timing) Timing.encode
    <> nullN 3
    <> contramap (^. #duration) Serializers.word32Le
    <> contramap (Probability.upper . view #probability) Serializers.word8
    <> nullN 1
    <> contramap (Probability.lower . view #probability) Serializers.word8
    <> nullN 1
    <> contramap (^. #resref1) Resref.encode
    <> contramap (^. #dice % Dice.dice % _1) Serializers.word8
    <> nullN 3
    <> contramap (^. #dice % Dice.dice % _2) Serializers.word8
    <> nullN 3
    <> contramap (^. #flags) Flags.encode
    <> contramap (^. #savebonus) SaveBonus.encode
    <> contramap (^. #special) Serializers.word32Le
    <> contramap (^. #school) School.encode
    <> nullN 15
    <> contramap (^. #resistdispel) ResistDispel.encode
    <> nullN 3
    <> contramap (^. #parameter3) Serializers.word32Le
    <> contramap (^. #parameter4) Serializers.word32Le
    <> nullN 8
    <> contramap (^. #resref2) Resref.encode
    <> nullN 40
    <> contramap (^. #projectile) Projectile.encode
    <> nullN 44
    <> contramap (^. #sectype) Sectype.encode
    <> nullN 63

{- | Serializer for an embedded t'Effect'. -}
{-# INLINE encode #-}
encode :: Serializers.Binary s => Serializer s Effect
encode
    =  value char8s "EFF V2.0"
    <> nullN 8
    <> encodeBody  
