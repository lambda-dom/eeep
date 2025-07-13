{- |
Module: Eeep.Types.Effect

The @Effect@ type.
-}

module Eeep.Types.Effect (
    -- * Types.
    Effect (..),

    -- * Parsers.
    parseEffect,

    -- * Testing.
    testEffect,
) where

-- Imports.
-- Base.
import Data.Bifunctor (Bifunctor (..))
import Data.Char (ord)
import Data.Typeable (Typeable)
import Data.Word (Word8)
import Data.Void (absurd)

-- Libraries.
import System.OsPath (OsPath)

-- non-Hackage libraries.
import Mono.Typeclasses.MonoFunctor (MonoFunctor (ElementOf))
import Mono.Typeclasses.MonoFoldable (MonoFoldable (monotoList))
import Trisagion.Typeclasses.HasOffset (HasOffset)
import Trisagion.Typeclasses.Splittable (Splittable (..))
import Trisagion.Types.ParseError (ParseError)
import Trisagion.Parser (Parser)
import Trisagion.Parsers.Combinators (skip)
import Trisagion.Parsers.Splittable (takeExact)
import Trisagion.Parsers.ParseError (throwParseError, onParseError, capture)
import Trisagion.Parsers.Word8 (word16Le)

-- Package.
import Eeep.Typeclasses.Binary (Reader (..), parseBinary)
import Eeep.Types.Opcode.OpType (OpType, decodeOpType32)
import Eeep.Types.Opcode.Parameter (Parameter (..), decodeParameter)
import Eeep.Types.Opcode.Power (Power, decodePower32)
import Eeep.Types.Opcode.Target (Target, decodeTarget32)
import Eeep.Types.Opcode.Timing (Timing, decodeTiming16)
import Eeep.Types.Opcode.Duration (Duration, decodeDuration)
import Eeep.Types.Opcode.Probability (Probability, parseProbability16)
import Eeep.Types.Opcode.Resref (Resref, parseResref)
import Eeep.Types.Opcode.ResistDispel (ResistDispel, parseResistDispel32)
import Eeep.Types.Opcode.DiceNumber (DiceNumber, parseDiceNumber)
import Eeep.Types.Opcode.DiceSides (DiceSides, parseDiceSides)
import Eeep.Types.Opcode.SaveFlags (SaveFlags, parseSaveFlags)
import Eeep.Types.Opcode.SaveBonus (SaveBonus, parseSaveBonus)
import Eeep.Types.Opcode.Special (Special, parseSpecial)
import Eeep.Types.Effect.Projectile (Projectile, parseProjectile)
import Eeep.Types.Effect.School (School, parseSchool)
import Eeep.Types.Effect.Sectype (Sectype, parseSectype)
import Eeep.IO (makePath)


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
    parser = parseEffect


{- | Parser for the signature header of an t'Effect'. -}
parseEffectSignature
    :: (HasOffset s, Splittable s, MonoFoldable (PrefixOf s), ElementOf (PrefixOf s) ~ Word8)
    => Parser s (ParseError EffectSignatureError) (PrefixOf s)
parseEffectSignature = do
    prefix <- first (fmap absurd) $ takeExact 8
    if monotoList prefix == (fromIntegral . ord <$> "EFF V2.0")
        then pure prefix
        else throwParseError EffectSignatureError

{- | Parser for the t'Effect' type. -}
parseEffect
    :: forall s . (HasOffset s, Splittable s, MonoFoldable (PrefixOf s), ElementOf (PrefixOf s) ~ Word8)
    => Parser s (ParseError EffectError) Effect
parseEffect = capture $ do
        _           <- onError parseEffectSignature
        _           <- skip (first (fmap absurd) $ takeExact 8)
        optype      <- onError decodeOpType32
        target      <- onError decodeTarget32
        power       <- onError decodePower32
        parameter1  <- onError decodeParameter
        parameter2  <- onError decodeParameter
        timing      <- onError decodeTiming16
        _           <- skip (first (fmap absurd) word16Le)
        duration    <- onError decodeDuration
        probability <- onError parseProbability16
        resource1   <- onError parseResref
        dicenumber  <- onError parseDiceNumber
        dicesides   <- onError parseDiceSides
        saveflags   <- onError parseSaveFlags
        savebonus   <- onError parseSaveBonus
        special     <- onError parseSpecial
        school      <- onError parseSchool
        _           <- skip (first (fmap absurd) $ takeExact 12)
        dispel      <- onError parseResistDispel32
        parameter3  <- onError decodeParameter
        parameter4  <- onError decodeParameter
        _           <- skip (first (fmap absurd) $ takeExact 8)
        resource2   <- onError parseResref
        resource3   <- onError parseResref
        _           <- skip (first (fmap absurd) $ takeExact 32)
        projectile  <- onError parseProjectile
        _           <- skip (first (fmap absurd) $ takeExact 44)
        sectype     <- onError parseSectype
        _           <- skip (first (fmap absurd) $ takeExact 60)
        pure Effect {..}
    where
        onError :: (Typeable e, Eq e, Show e) => Parser s (ParseError e) a -> Parser s (ParseError EffectError) a
        onError = onParseError EffectError


{- | Helper to test t'Effect' parser. -}
testEffect :: String -> IO ()
testEffect str = do
        path <- makePath str
        x <- parse path
        print x
    where
        parse :: OsPath -> IO (Either (ParseError EffectError) Effect)
        parse = parseBinary
