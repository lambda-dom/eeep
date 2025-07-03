{- |
Module: Eeep.Types.Effect

The @Effect@ type.
-}

module Eeep.Types.Effect (
    -- * Types.
    Effect (..),

    -- * Parsers.
    getHeader,
    getOptype,
    getParameter,
    getTarget,
    getPower,
    getTiming,
    getDuration,
    getProbability,
    getResref,
    getDiceNumber,
    getDiceSides,
    getSaveFlags,
    getSaveBonus,
    getSpecial,
    getResistDispel,
    getProjectile,
    getSchool,
    getSectype,
) where

-- Imports.
-- Base.
import Data.Bifunctor (Bifunctor (..))
import Data.Char (ord)
import Data.Int (Int32)
import Data.Word (Word8, Word32, Word16)
import Data.Void (absurd)

-- non-Hackage libraries.
import Mono.Typeclasses.MonoFunctor (MonoFunctor (ElementOf))
import Mono.Typeclasses.MonoFoldable (MonoFoldable (monotoList))
import Trisagion.Typeclasses.HasOffset (HasOffset)
import Trisagion.Typeclasses.Splittable (Splittable (..))
import Trisagion.Types.ParseError (ParseError)
import Trisagion.Parser (Parser)
import Trisagion.Parsers.Splittable (takeExact)
import Trisagion.Parsers.ParseError (throwParseError)
import Trisagion.Parsers.Word8 (word32Le, word16Le, word64Le)

-- Package.
import Eeep.Types.Opcode.OpType (OpType, toOpType)
import Eeep.Types.Opcode.Parameter (Parameter (..))
import Eeep.Types.Opcode.Power (Power, toPower)
import Eeep.Types.Opcode.Target (Target, toTarget)
import Eeep.Types.Opcode.Timing (Timing, toTiming)
import Eeep.Types.Opcode.Duration (Duration (..))
import Eeep.Types.Opcode.Probability (Probability, toProbability)
import Eeep.Types.Opcode.Resref (Resref (..))
import Eeep.Types.Opcode.ResistDispel (ResistDispel, toResistDispel)
import Eeep.Types.Opcode.DiceNumber (DiceNumber (..))
import Eeep.Types.Opcode.DiceSides (DiceSides (..))
import Eeep.Types.Opcode.SaveFlags (SaveFlags, toSaveFlags)
import Eeep.Types.Opcode.SaveBonus (SaveBonus, toSaveBonus)
import Eeep.Types.Opcode.Special (Special (..))
import Eeep.Types.Effect.Projectile (Projectile (..))
import Eeep.Types.Effect.School (School (..))
import Eeep.Types.Effect.Sectype (Sectype (..))


{- | The t'EffectError' type. -}
data EffectError
    = HeaderError
    | OpTypeError !Word32
    | TargetError !Word32
    | PowerError !Word32
    | TimingError !Word32
    | ProbabilityError !Word16 !Word16
    | SaveBonusError !Int32
    | ResistDispelError !Word32
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


{- | Parser for the signature header of an Effect. -}
getHeader
    :: (HasOffset s, Splittable s, MonoFoldable (PrefixOf s), ElementOf (PrefixOf s) ~ Word8)
    => Parser s (ParseError EffectError) (PrefixOf s)
getHeader = do
    prefix <- first (fmap absurd) $ takeExact 8
    if monotoList prefix == (fromIntegral . ord <$> "EFF V2.0")
        then pure prefix
        else throwParseError HeaderError

{- | Parser for the t'OpType' type. -}
getOptype
    :: (HasOffset s, Splittable s, MonoFoldable (PrefixOf s), ElementOf (PrefixOf s) ~ Word8)
    => Parser s (ParseError EffectError) OpType
getOptype = do
    n <- first (fmap absurd) word32Le
    maybe (throwParseError $ OpTypeError n) pure (toOpType n)

{- | Parser for the t'Parameter' type. -}
getParameter
    :: (Splittable s, MonoFoldable (PrefixOf s), ElementOf (PrefixOf s) ~ Word8)
    => Parser s (ParseError EffectError) Parameter
getParameter = Parameter <$> first (fmap absurd) word32Le

{- | Parser for the t'Target' type. -}
getTarget
    :: (HasOffset s, Splittable s, MonoFoldable (PrefixOf s), ElementOf (PrefixOf s) ~ Word8)
    => Parser s (ParseError EffectError) Target
getTarget = do
    n <- first (fmap absurd) word32Le
    maybe (throwParseError $ TargetError n) pure (toTarget n)

{- | Parser for the t'Power' type. -}
getPower
    :: (HasOffset s, Splittable s, MonoFoldable (PrefixOf s), ElementOf (PrefixOf s) ~ Word8)
    => Parser s (ParseError EffectError) Power
getPower = do
    n <- first (fmap absurd) word32Le
    maybe (throwParseError $ PowerError n) pure (toPower n)

{- | Parser for the t'Timing' type. -}
getTiming
    :: (HasOffset s, Splittable s, MonoFoldable (PrefixOf s), ElementOf (PrefixOf s) ~ Word8)
    => Parser s (ParseError EffectError) Timing
getTiming = do
    n <- bimap (fmap absurd) fromIntegral word16Le
    maybe (throwParseError $ TimingError n) pure (toTiming n)

{- | Parser for the t'Duration' type. -}
getDuration
    :: (Splittable s, MonoFoldable (PrefixOf s), ElementOf (PrefixOf s) ~ Word8)
    => Parser s (ParseError EffectError) Duration
getDuration = Duration <$> first (fmap absurd) word32Le

{- | Parser for the t'Probability' type. -}
getProbability
    :: (HasOffset s, Splittable s, MonoFoldable (PrefixOf s), ElementOf (PrefixOf s) ~ Word8)
    => Parser s (ParseError EffectError) Probability
getProbability = do
    n <- first (fmap absurd) word16Le
    m <- first (fmap absurd) word16Le
    maybe (throwParseError $ ProbabilityError n m) pure (toProbability n m)

{- | Parser for the t'Resref' type. -}
getResref
    :: (Splittable s, MonoFoldable (PrefixOf s), ElementOf (PrefixOf s) ~ Word8)
    => Parser s (ParseError EffectError) Resref
getResref = Resref <$> first (fmap absurd) word64Le

{- | Parser for the t'DiceNumber' type. -}
getDiceNumber
    :: (Splittable s, MonoFoldable (PrefixOf s), ElementOf (PrefixOf s) ~ Word8)
    => Parser s (ParseError EffectError) DiceNumber
getDiceNumber = DiceNumber <$> first (fmap absurd) word32Le

{- | Parser for the t'DiceSides' type. -}
getDiceSides
    :: (Splittable s, MonoFoldable (PrefixOf s), ElementOf (PrefixOf s) ~ Word8)
    => Parser s (ParseError EffectError) DiceSides
getDiceSides = DiceSides <$> first (fmap absurd) word32Le

{- | Parser for the t'SaveFlags' type. -}
getSaveFlags
    :: (Splittable s, MonoFoldable (PrefixOf s), ElementOf (PrefixOf s) ~ Word8)
    => Parser s (ParseError EffectError) SaveFlags
getSaveFlags = toSaveFlags <$> first (fmap absurd) word32Le

{- | Parser for the t'SaveBonus' type. -}
getSaveBonus
    :: (HasOffset s, Splittable s, MonoFoldable (PrefixOf s), ElementOf (PrefixOf s) ~ Word8)
    => Parser s (ParseError EffectError) SaveBonus
getSaveBonus = do
    n <- bimap (fmap absurd) fromIntegral word32Le
    maybe (throwParseError $ SaveBonusError n) pure (toSaveBonus n)

{- | Parser for the t'Special' type. -}
getSpecial
    :: (Splittable s, MonoFoldable (PrefixOf s), ElementOf (PrefixOf s) ~ Word8)
    => Parser s (ParseError EffectError) Special
getSpecial = Special <$> first (fmap absurd) word32Le

{- | Parser for the t'ResistDispel' type. -}
getResistDispel
    :: (HasOffset s, Splittable s, MonoFoldable (PrefixOf s), ElementOf (PrefixOf s) ~ Word8)
    => Parser s (ParseError EffectError) ResistDispel
getResistDispel = do
    n <- first (fmap absurd) word32Le
    maybe (throwParseError $ ResistDispelError n) pure (toResistDispel n)

{- | Parser for the t'Projectile' type. -}
getProjectile
    :: (Splittable s, MonoFoldable (PrefixOf s), ElementOf (PrefixOf s) ~ Word8)
    => Parser s (ParseError EffectError) Projectile

getProjectile = Projectile <$> first (fmap absurd) word32Le

{- | Parser for the t'School' type. -}
getSchool
    :: (Splittable s, MonoFoldable (PrefixOf s), ElementOf (PrefixOf s) ~ Word8)
    => Parser s (ParseError EffectError) School
getSchool = School <$> first (fmap absurd) word32Le

{- | Parser for the t'Sectype' type. -}
getSectype
    :: (Splittable s, MonoFoldable (PrefixOf s), ElementOf (PrefixOf s) ~ Word8)
    => Parser s (ParseError EffectError) Sectype
getSectype = Sectype <$> first (fmap absurd) word32Le
