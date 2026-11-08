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
import Data.Word (Word8, Word16)

-- Libraries.
import Control.Monad.Except (MonadError (..))
import Data.ByteString.Short (ShortByteString)
import Optics.Core ((%), (^.), review, coercedTo, preview, view)

-- non-Hackage libraries.
import Mono.Typeclasses.MonoFoldable (MonoFoldable (..))
import Trisagion.Utils.Either ((:+:))
import qualified Trisagion.Typeclasses.Parsers.Binary as Parsers (Binary (..))
import Trisagion.Parser (Parser)
import Trisagion.Parsers.Source (InputError, ValidationError (..), matchFoldable)
import Trisagion.Parsers.Split (skipExact)
import qualified Trisagion.Typeclasses.Serializers.Binary as Serializers (Binary (..))
import Trisagion.Serializer (Serializer)
import Trisagion.Typeclasses.Serializers.Binary (nulls, char8s)

-- Package.
import Eeep.Types.Shared.Dice (Dice)
import qualified Eeep.Types.Shared.Dice as Dice (decode, encode)
import Eeep.Types.Shared.Resref (Resref)
import qualified Eeep.Types.Shared.Resref as Resref (Error (..), decode, encode)
import Eeep.Types.Shared.Projectile (Projectile)
import qualified Eeep.Types.Shared.Projectile as Projectile (decode, encode)
import Eeep.Types.Shared.School (School)
import qualified Eeep.Types.Shared.School as School (decode, encode)
import Eeep.Types.Shared.Sectype (Sectype)
import qualified Eeep.Types.Shared.Sectype as Sectype (decode, encode)
import Eeep.Types.Opcode.OpType (OpType)
import qualified Eeep.Types.Opcode.OpType as OpType (Error (..), decode, encode)
import Eeep.Types.Opcode.Parameter (Parameter)
import qualified Eeep.Types.Opcode.Parameter as Parameter (decode, encode)
import Eeep.Types.Opcode.Power (Power)
import qualified Eeep.Types.Opcode.Power as Power (Error (..), decode, encode)
import Eeep.Types.Opcode.Target (Target)
import qualified Eeep.Types.Opcode.Target as Target (Error (..), decode, encode)
import Eeep.Types.Opcode.Timing (Timing)
import qualified Eeep.Types.Opcode.Timing as Timing (Error (..), decode, encode)
import Eeep.Types.Opcode.Duration (Duration)
import qualified Eeep.Types.Opcode.Duration as Duration (decode, encode)
import Eeep.Types.Opcode.Probability (Probability)
import qualified Eeep.Types.Opcode.Probability as Probability (probability, upper, lower)
import Eeep.Types.Opcode.ResistDispel (ResistDispel)
import qualified Eeep.Types.Opcode.ResistDispel as ResistDispel (Error (..), decode, encode)
import Eeep.Types.Opcode.SaveFlags (SaveFlags)
import qualified Eeep.Types.Opcode.SaveFlags as SaveFlags (decode, encode)
import Eeep.Types.Opcode.SaveBonus (SaveBonus)
import qualified Eeep.Types.Opcode.SaveBonus as SaveBonus (Error (..), decode, encode)
import Eeep.Types.Opcode.Special (Special)
import qualified Eeep.Types.Opcode.Special as Special (decode, encode)
import Trisagion.Serializers.Combinators (value)


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
    | ResrefError                      !ShortByteString
    | SaveBonusError    {-# UNPACK #-} !Int32
    deriving stock (Eq, Ord, Generic, Show)


{- | The @Effect@ type. -}
data Effect = Effect {
    optype       :: {-# UNPACK #-} !OpType,
    parameter1   :: {-# UNPACK #-} !Parameter,
    parameter2   :: {-# UNPACK #-} !Parameter,
    parameter3   :: {-# UNPACK #-} !Parameter,
    parameter4   :: {-# UNPACK #-} !Parameter,
    power        :: {-# UNPACK #-} !Power,
    target       :: {-# UNPACK #-} !Target,
    timing       :: {-# UNPACK #-} !Timing,
    duration     :: {-# UNPACK #-} !Duration,
    probability  :: {-# UNPACK #-} !Probability,
    resistdispel :: {-# UNPACK #-} !ResistDispel,
    resref1      :: {-# UNPACK #-} !Resref,
    resref2      :: {-# UNPACK #-} !Resref,
    resref3      :: {-# UNPACK #-} !Resref,
    dice         :: {-# UNPACK #-} !Dice,
    saveflags    :: {-# UNPACK #-} !SaveFlags,
    savebonus    :: {-# UNPACK #-} !SaveBonus,
    projectile   :: {-# UNPACK #-} !Projectile,
    school       :: {-# UNPACK #-} !School,
    sectype      :: {-# UNPACK #-} !Sectype,
    special      :: {-# UNPACK #-} !Special
} deriving stock (Eq, Show, Generic)


{- | Parser for the @t'Effect'@ signature. -}
{-# INLINE signature #-}
signature
    :: forall b s . (Parsers.Binary b s)
    => Parser s (Error :+: InputError) ()
signature =
    first
        (first (\ (ValidationError n) -> SignatureError n))
        (matchFoldable (fmap (fromIntegral @Int @Word8 . ord) "EFF V2.0"))


-- | Parser for t'Probability'. Not exported.
{-# INLINE probParser #-}
probParser :: Parsers.Binary b s => Parser s (Error :+: InputError) Probability
probParser = do
    upper <- first Right Parsers.word8
    _     <- first Right $ skipExact 1
    lower <- first Right Parsers.word8
    _     <- first Right $ skipExact 1
    case preview Probability.probability (lower, upper) of
        Just x  -> pure x
        Nothing -> throwError . Left $ ProbabilityError lower upper


{- | Parser for an embedded @t'Effect'@. -}
{-# INLINEABLE decodeBody #-}
decodeBody :: (Parsers.Binary b s, MonoFoldable Word8 b) => Parser s (Error :+: InputError) Effect
decodeBody = do
    optype       <- first (first (review $ #_OpTypeError % coercedTo)) OpType.decode
    _            <- first Right $ skipExact 2
    target       <- first (first (review $ #_TargetError % coercedTo)) Target.decode
    _            <- first Right $ skipExact 3
    power        <- first (first (review $ #_PowerError % coercedTo)) Power.decode
    _            <- first Right $ skipExact 3
    parameter1   <- first Right Parameter.decode
    parameter2   <- first Right Parameter.decode
    timing       <- first (first (review $ #_TimingError % coercedTo)) Timing.decode
    _            <- first Right $ skipExact 3
    duration     <- first Right Duration.decode
    probability  <- probParser
    resref1      <- first (first (review $ #_ResrefError % coercedTo)) Resref.decode
    dice         <- first Right Dice.decode
    saveflags    <- first Right SaveFlags.decode
    savebonus    <- first (first (review $ #_SaveBonusError % coercedTo)) SaveBonus.decode
    special      <- first Right Special.decode
    school       <- first Right School.decode
    _            <- first Right $ skipExact 14
    resistdispel <- first (first (review $ #_ResistDispelError % coercedTo)) ResistDispel.decode
    _            <- first Right $ skipExact 3
    parameter3   <- first Right Parameter.decode
    parameter4   <- first Right Parameter.decode
    _            <- first Right $ skipExact 8
    resref2      <- first (first (review $ #_ResrefError % coercedTo)) Resref.decode
    resref3      <- first (first (review $ #_ResrefError % coercedTo)) Resref.decode
    _            <- first Right $ skipExact 32
    projectile   <- first Right Projectile.decode
    _            <- first Right $ skipExact 44
    sectype      <- first Right Sectype.decode
    _            <- first Right $ skipExact 62
    pure Effect {..}

{- | Parser for a file @t'Effect'@. -}
{-# INLINE decode #-}
decode :: (Parsers.Binary b s, MonoFoldable Word8 b) => Parser s (Error :+: InputError) Effect
decode = do
    _ <- signature
    _ <- first Right $ skipExact 8
    decodeBody
    
{- | Serializer for an embedded t'Effect'. -}
{-# INLINEABLE encodeBody #-}
encodeBody :: Serializers.Binary b s => Serializer s Effect
encodeBody
    =  contramap (^. #optype) OpType.encode
    <> nulls 2
    <> contramap (^. #target) Target.encode
    <> nulls 3
    <> contramap (^. #power) Power.encode
    <> nulls 3
    <> contramap (^. #parameter1) Parameter.encode
    <> contramap (^. #parameter2) Parameter.encode
    <> contramap (^. #timing) Timing.encode
    <> nulls 3
    <> contramap (^. #duration) Duration.encode
    <> contramap (Probability.upper . view #probability) Serializers.word8
    <> nulls 1
    <> contramap (Probability.lower . view #probability) Serializers.word8
    <> nulls 1
    <> contramap (^. #resref1) Resref.encode
    <> contramap (^. #dice) Dice.encode
    <> contramap (^. #saveflags) SaveFlags.encode
    <> contramap (^. #savebonus) SaveBonus.encode
    <> contramap (^. #special) Special.encode
    <> contramap (^. #school) School.encode
    <> nulls 12
    <> contramap (^. #resistdispel) ResistDispel.encode
    <> nulls 3
    <> contramap (^. #parameter3) Parameter.encode
    <> contramap (^. #parameter4) Parameter.encode
    <> nulls 8
    <> contramap (^. #resref2) Resref.encode
    <> contramap (^. #resref3) Resref.encode
    <> nulls 32
    <> contramap (^. #projectile) Projectile.encode
    <> nulls 46
    <> contramap (^. #sectype) Sectype.encode
    <> nulls 60

{- | Serializer for an embedded t'Effect'. -}
{-# INLINE encode #-}
encode :: Serializers.Binary b s => Serializer s Effect
encode
    =  value char8s "EFF V2.0"
    <> nulls 8
    <> encodeBody  
