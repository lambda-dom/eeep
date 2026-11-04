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
    decode,
) where

-- Imports.
-- Base.
import GHC.Generics (Generic)
import Data.Bifunctor (Bifunctor (..))
import Data.Char (ord)
import Data.Int (Int32)
import Data.Word (Word8, Word16)

-- Libraries.
import Control.Monad.Except (MonadError (..))
import Data.ByteString.Short (ShortByteString)
import Optics.Core ((%), review, coercedTo, preview)

-- non-Hackage libraries.
import Mono.Typeclasses.MonoFoldable (MonoFoldable (..))
import Trisagion.Utils.Either ((:+:))
import qualified Trisagion.Typeclasses.Parsers.Binary as Parsers (Binary (..))
import Trisagion.Parser (Parser)
import Trisagion.Parsers.Combinators (skip, validate)
import Trisagion.Parsers.Source (InputError)
import Trisagion.Parsers.Split (takeExact)

-- Package.
import Eeep.Types.Opcode.OpType (OpType)
import qualified Eeep.Types.Opcode.OpType as OpType (Error (..), decode)
import Eeep.Types.Opcode.Parameter (Parameter)
import qualified Eeep.Types.Opcode.Parameter as Parameter (decode)
import Eeep.Types.Opcode.Power (Power)
import qualified Eeep.Types.Opcode.Power as Power (Error (..), decode)
import Eeep.Types.Opcode.Target (Target)
import qualified Eeep.Types.Opcode.Target as Target (Error (..), decode)
import Eeep.Types.Opcode.Timing (Timing)
import qualified Eeep.Types.Opcode.Timing as Timing (Error (..), decode)
import Eeep.Types.Opcode.Duration (Duration)
import qualified Eeep.Types.Opcode.Duration as Duration (decode)
import Eeep.Types.Opcode.Probability (Probability)
import qualified Eeep.Types.Opcode.Probability as Probability (probability)
import Eeep.Types.Opcode.ResistDispel (ResistDispel)
import qualified Eeep.Types.Opcode.ResistDispel as ResistDispel (Error (..), decode)
import Eeep.Types.Opcode.Resref (Resref)
import qualified Eeep.Types.Opcode.Resref as Resref (Error (..), decode)
import Eeep.Types.Opcode.Dice (Dice)
import qualified Eeep.Types.Opcode.Dice as Dice (decode)
import Eeep.Types.Opcode.SaveFlags (SaveFlags)
import qualified Eeep.Types.Opcode.SaveFlags as SaveFlags (decode)
import Eeep.Types.Opcode.SaveBonus (SaveBonus)
import qualified Eeep.Types.Opcode.SaveBonus as SaveBonus (Error (..), decode)
import Eeep.Types.Effect.Projectile (Projectile)
import qualified Eeep.Types.Effect.Projectile as Projectile (decode)
import Eeep.Types.Effect.School (School)
import qualified Eeep.Types.Effect.School as School (decode)
import Eeep.Types.Effect.Sectype (Sectype)
import qualified Eeep.Types.Effect.Sectype as Sectype (decode)
import Eeep.Types.Opcode.Special (Special)
import qualified Eeep.Types.Opcode.Special as Special (decode)


{- | The t'Effect' @Error@ type.

note(s):

    * To be imported qualified.
-}
data Error
    = SignatureError
    | OpTypeError       {-# UNPACK #-} !Word16
    | TargetError       {-# UNPACK #-} !Word8
    | PowerError        {-# UNPACK #-} !Word8
    | TimingError       {-# UNPACK #-} !Word8
    | ResistDispelError {-# UNPACK #-} !Word8
    | ProbabilityError  {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8
    | ResrefError       {-# UNPACK #-} !ShortByteString
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


-- | Parser for the t'Effect' signature. Not exported.
{-# INLINE signature #-}
signature
    :: forall b s . (Parsers.Binary b s, MonoFoldable Word8 b)
    => Parser s (Error :+: InputError) ()
signature = skip $ validate v (takeExact 8)
    where
        v :: b -> Error :+: b
        v xs =
            if fmap (fromIntegral @Int @Word8 . ord) "EFF V2.0" == monotoList xs
            then Left $ SignatureError
            else Right xs


-- | Parser for t'Probability'. Not exported.
{-# INLINE probParser #-}
probParser :: Parsers.Binary b s => Parser s (Error :+: InputError) Probability
probParser = do
    upper <- first Right Parsers.word8
    _     <- skip (first Right $ takeExact 1)
    lower <- first Right Parsers.word8
    _     <- skip (first Right $ takeExact 1)
    case preview Probability.probability (lower, upper) of
        Just x  -> pure x
        Nothing -> throwError . Left $ ProbabilityError lower upper


{- | Parser for the @t'Effect'@ type. -}
{-# INLINEABLE decode #-}
decode :: (Parsers.Binary b s, MonoFoldable Word8 b) => Parser s (Error :+: InputError) Effect
decode = do
    _            <- signature
    _            <- skip (first Right $ takeExact 8)
    optype       <- first (first (review $ #_OpTypeError % coercedTo)) OpType.decode
    _            <- skip (first Right $ takeExact 2)
    target       <- first (first (review $ #_TargetError % coercedTo)) Target.decode
    _            <- skip (first Right $ takeExact 3)
    power        <- first (first (review $ #_PowerError % coercedTo)) Power.decode
    _            <- skip (first Right $ takeExact 3)
    parameter1   <- first Right Parameter.decode
    parameter2   <- first Right Parameter.decode
    timing       <- first (first (review $ #_TimingError % coercedTo)) Timing.decode
    _            <- skip (first Right $ takeExact 3)
    duration     <- first Right Duration.decode
    probability  <- probParser
    resref1      <- first (first (review $ #_ResrefError % coercedTo)) Resref.decode
    dice         <- first Right Dice.decode
    saveflags    <- first Right SaveFlags.decode
    savebonus    <- first (first (review $ #_SaveBonusError % coercedTo)) SaveBonus.decode
    special      <- first Right Special.decode
    school       <- first Right School.decode
    _            <- skip (first Right $ takeExact 12)
    resistdispel <- first (first (review $ #_ResistDispelError % coercedTo)) ResistDispel.decode
    _            <- skip (first Right $ takeExact 3)
    parameter3   <- first Right Parameter.decode
    parameter4   <- first Right Parameter.decode
    _            <- skip (first Right $ takeExact 8)
    resref2      <- first (first (review $ #_ResrefError % coercedTo)) Resref.decode
    resref3      <- first (first (review $ #_ResrefError % coercedTo)) Resref.decode
    _            <- skip (first Right $ takeExact 32)
    projectile   <- first Right Projectile.decode
    _            <- skip (first Right $ takeExact 44)
    sectype      <- first Right Sectype.decode
    _            <- skip (first Right $ takeExact 60)
    pure Effect {..}
