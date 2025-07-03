{- |
Module: Eeep.Types.Effect

The @Effect@ type.
-}

module Eeep.Types.Effect (
    -- * Types.
    Effect (..),

    -- * Parsers.
    header,
) where

-- Imports.
-- Base.
import Data.Bifunctor (Bifunctor (..))
import Data.Char (ord)
import Data.Void (absurd)
import Data.Word (Word8)

-- non-Hackage libraries.
import Mono.Typeclasses.MonoFunctor (MonoFunctor (ElementOf))
import Mono.Typeclasses.MonoFoldable (MonoFoldable (monotoList))
import Trisagion.Typeclasses.HasOffset (HasOffset)
import Trisagion.Typeclasses.Splittable (Splittable (..))
import Trisagion.Types.ParseError (ParseError)
import Trisagion.Parser (Parser)
import Trisagion.Parsers.Splittable (takeExact)
import Trisagion.Parsers.ParseError (throwParseError)

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


{- | The t'EffectError' type. -}
data EffectError = HeaderError
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
    dicethrown  :: {-# UNPACK #-} !DiceNumber,
    dicesides   :: {-# UNPACK #-} !DiceSides,
    flags       :: {-# UNPACK #-} !SaveFlags,
    savebonus   :: {-# UNPACK #-} !SaveBonus,
    projectile  :: {-# UNPACK #-} !Projectile,
    school      :: {-# UNPACK #-} !School,
    sectype     :: {-# UNPACK #-} !Sectype,
    special     :: {-# UNPACK #-} !Special
    } deriving stock (Eq, Show)


-- Parsers.
header
    :: (HasOffset s, Splittable s, MonoFoldable (PrefixOf s), ElementOf (PrefixOf s) ~ ElementOf s, ElementOf s ~ Word8)
    => Parser s (ParseError EffectError) (PrefixOf s)
header = do
    prefix <- first (fmap absurd) $ takeExact 8
    if monotoList prefix == (fromIntegral . ord <$> "2DA V1.0")
        then pure prefix
        else throwParseError HeaderError
