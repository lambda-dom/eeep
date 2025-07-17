{-# LANGUAGE RecordWildCards #-}

{- |
Module: Eeep.Types.Effect

The @Effect@ type.
-}

module Eeep.Types.Effect (
    -- * Error types.
    EffectError (..),

    -- * Types.
    Effect,
) where

-- Imports.
-- Base.
import Data.Bifunctor (Bifunctor (..))
import Data.Functor.Contravariant (Contravariant (..))
import Data.Char (ord)
import Data.Typeable (Typeable)
import Data.Word (Word8)
import Data.Void (absurd)

-- Libraries.
import qualified Data.ByteString as Bytes (replicate)

-- non-Hackage libraries.
import Mono.Typeclasses.MonoFunctor (MonoFunctor (ElementOf))
import Mono.Typeclasses.MonoFoldable (MonoFoldable (monotoList))
import Trisagion.Typeclasses.HasOffset (HasOffset)
import Trisagion.Typeclasses.Splittable (Splittable (..))
import Trisagion.Typeclasses.Binary (Binary)
import Trisagion.Types.ParseError (ParseError)
import Trisagion.Parser (Parser)
import Trisagion.Parsers.Combinators (skip)
import Trisagion.Parsers.Splittable (takeExact)
import Trisagion.Parsers.ParseError (throwParseError, onParseError, capture)
import Trisagion.Parsers.Word8 (word16Le)
import Trisagion.Serializer (Serializer, serialize, (|*>))
import qualified Trisagion.Serializers.Binary as Binary (string, bytestring, word16Le)

-- Package.
import Eeep.Typeclasses.Binary (Reader (..), Writer (..))
import Eeep.Types.Opcode.OpType (OpType, decodeOpType32, encodeOpType32)
import Eeep.Types.Opcode.Parameter (Parameter, decodeParameter, encodeParameter)
import Eeep.Types.Opcode.Power (Power, decodePower32, encodePower32)
import Eeep.Types.Opcode.Target (Target, decodeTarget32, encodeTarget32)
import Eeep.Types.Opcode.Timing (Timing, decodeTiming16, encodeTiming16)
import Eeep.Types.Opcode.Duration (Duration, decodeDuration, encodeDuration)
import Eeep.Types.Opcode.Probability (Probability, decodeProbability16, encodeProbability16)
import Eeep.Types.Opcode.Resref (Resref, decodeResref, encodeResref)
import Eeep.Types.Opcode.ResistDispel (ResistDispel, decodeResistDispel32, encodeResistDispel32)
import Eeep.Types.Opcode.DiceNumber (DiceNumber, decodeDiceNumber, encodeDiceNumber)
import Eeep.Types.Opcode.DiceSides (DiceSides, decodeDiceSides, encodeDiceSides)
import Eeep.Types.Opcode.SaveFlags (SaveFlags, decodeSaveFlags, encodeSaveFlags)
import Eeep.Types.Opcode.SaveBonus (SaveBonus, decodeSaveBonus, encodeSaveBonus)
import Eeep.Types.Opcode.Special (Special, decodeSpecial, encodeSpecial)
import Eeep.Types.Effect.School (School, decodeSchool, encodeSchool)
import Eeep.Types.Effect.Sectype (Sectype, decodeSectype, encodeSectype)
import Eeep.Types.Effect.Projectile (Projectile, decodeProjectile, encodeProjectile)


{- | The t'EffectError' type. -}
data EffectSignatureError = EffectSignatureError
    deriving stock (Eq, Show)

{- | The t'EffectError' type. -}
data EffectError = EffectError
    deriving stock (Eq, Show)


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
    dicenumber  :: {-# UNPACK #-} !DiceNumber,
    dicesides   :: {-# UNPACK #-} !DiceSides,
    saveflags   :: {-# UNPACK #-} !SaveFlags,
    savebonus   :: {-# UNPACK #-} !SaveBonus,
    projectile  :: {-# UNPACK #-} !Projectile,
    school      :: {-# UNPACK #-} !School,
    sectype     :: {-# UNPACK #-} !Sectype,
    special     :: {-# UNPACK #-} !Special
    } deriving stock (Eq, Show)


-- Instances.
instance Reader (ParseError EffectError) Effect where
    parser = decodeEffect

instance Writer Effect where
    serializer = encodeEffect


{- | Parser for the signature header of an t'Effect'. -}
decodeEffectSignature
    :: (HasOffset s, Splittable s, MonoFoldable (PrefixOf s), ElementOf (PrefixOf s) ~ Word8)
    => Parser s (ParseError EffectSignatureError) (PrefixOf s)
decodeEffectSignature = do
    prefix <- first (fmap absurd) $ takeExact 8
    if monotoList prefix == (fromIntegral . ord <$> "EFF V2.0")
        then pure prefix
        else throwParseError EffectSignatureError

{- | Parser for the t'Effect' type. -}
decodeEffect
    :: forall s . (HasOffset s, Splittable s, MonoFoldable (PrefixOf s), ElementOf (PrefixOf s) ~ Word8)
    => Parser s (ParseError EffectError) Effect
decodeEffect = capture $ do
        _           <- onError decodeEffectSignature
        _           <- skip (first (fmap absurd) $ takeExact 8)
        optype      <- onError decodeOpType32
        target      <- onError decodeTarget32
        power       <- onError decodePower32
        parameter1  <- onError decodeParameter
        parameter2  <- onError decodeParameter
        timing      <- onError decodeTiming16
        _           <- skip (first (fmap absurd) word16Le)
        duration    <- onError decodeDuration
        probability <- onError decodeProbability16
        resource1   <- onError decodeResref
        dicenumber  <- onError decodeDiceNumber
        dicesides   <- onError decodeDiceSides
        saveflags   <- onError decodeSaveFlags
        savebonus   <- onError decodeSaveBonus
        special     <- onError decodeSpecial
        school      <- onError decodeSchool
        _           <- skip (first (fmap absurd) $ takeExact 12)
        dispel      <- onError decodeResistDispel32
        parameter3  <- onError decodeParameter
        parameter4  <- onError decodeParameter
        _           <- skip (first (fmap absurd) $ takeExact 8)
        resource2   <- onError decodeResref
        resource3   <- onError decodeResref
        _           <- skip (first (fmap absurd) $ takeExact 32)
        projectile  <- onError decodeProjectile
        _           <- skip (first (fmap absurd) $ takeExact 44)
        sectype     <- onError decodeSectype
        _           <- skip (first (fmap absurd) $ takeExact 60)
        pure Effect {..}
    where
        onError :: (Typeable e, Eq e, Show e) => Parser s (ParseError e) a -> Parser s (ParseError EffectError) a
        onError = onParseError EffectError


{- | Serializer for an t'Effect'. -}
encodeEffect :: Binary m => Serializer m Effect
encodeEffect
    =   serialize Binary.string "EFF V2.0"
    |*> serialize Binary.bytestring (Bytes.replicate 8 0)
    |*> contramap optype encodeOpType32
    <>  contramap target encodeTarget32
    <>  contramap power encodePower32
    <>  contramap parameter1 encodeParameter
    <>  contramap parameter2 encodeParameter
    <>  contramap timing encodeTiming16
    <>  contramap (const 0) Binary.word16Le
    <>  contramap duration encodeDuration
    <>  contramap probability encodeProbability16
    <>  contramap resource1 encodeResref
    <>  contramap dicenumber encodeDiceNumber
    <>  contramap dicesides encodeDiceSides
    <>  contramap saveflags encodeSaveFlags
    <>  contramap savebonus encodeSaveBonus
    <>  contramap special encodeSpecial
    <>  contramap school encodeSchool
    <>  contramap (const (Bytes.replicate 12 0)) Binary.bytestring
    <>  contramap dispel encodeResistDispel32
    <>  contramap parameter3 encodeParameter
    <>  contramap parameter4 encodeParameter
    <>  contramap (const (Bytes.replicate 8 0)) Binary.bytestring
    <>  contramap resource2 encodeResref
    <>  contramap resource3 encodeResref
    <>  contramap (const (Bytes.replicate 32 0)) Binary.bytestring
    <>  contramap projectile encodeProjectile
    <>  contramap (const (Bytes.replicate 44 0)) Binary.bytestring
    <>  contramap sectype encodeSectype
    <>  contramap (const (Bytes.replicate 60 0)) Binary.bytestring
