{- |
Module: Eeep.Types.Opcode.Power

The @Power@ type.
-}

module Eeep.Types.Opcode.Power (
    -- * Error types.
    PowerError (..),

    -- * Types.
    Power,

    -- ** Constructors.
    toPower,

    -- * Parsers.
    parsePower8,

    -- * Types.
    parsePower32,
) where

-- Imports.
-- Base.
import Data.Bifunctor (Bifunctor(..))
import Data.Ix (Ix)
import Data.Void (absurd)
import Data.Word (Word8, Word32)

-- non-Hackage libraries.
import Mono.Typeclasses.MonoFunctor (ElementOf)
import Mono.Typeclasses.MonoFoldable (MonoFoldable)
import Trisagion.Types.ParseError (ParseError)
import Trisagion.Typeclasses.HasOffset (HasOffset)
import Trisagion.Typeclasses.Splittable (Splittable (PrefixOf))
import Trisagion.Parser (Parser)
import Trisagion.Parsers.ParseError (throwParseError, capture)
import Trisagion.Parsers.Word8 (word8, word32Le)


{- | The t'PowerError' type. -}
newtype PowerError = PowerError Word32
    deriving stock (Eq, Show)


{- | Refinement type containing values in the interval @[0 .. 10]@. -}
newtype Power = Power Word8
    deriving stock (Eq, Ord, Ix, Show)

-- Instances.
instance Bounded Power where
    {-# INLINE minBound #-}
    minBound :: Power
    minBound = Power 0

    {-# INLINE maxBound #-}
    maxBound :: Power
    maxBound = Power 10


{- | Smart constructor for the t'Power' type.-}
{-# INLINE toPower #-}
toPower :: Word8 -> Maybe Power
toPower n = if n <= 10 then Just $ Power n else Nothing


{- | Parse a t'Power' from a single 'Word8'. -}
parsePower8 :: (HasOffset s, ElementOf s ~ Word8) => Parser s (ParseError PowerError) Power
parsePower8 = capture $ do
    n <- first (fmap absurd) word8
    maybe (throwParseError . PowerError . fromIntegral $ n) pure (toPower n)

{- | Parse a t'Power' from a single 'Word32'. -}
parsePower32
    :: (HasOffset s, Splittable s, MonoFoldable (PrefixOf s), ElementOf (PrefixOf s) ~ Word8)
    => Parser s (ParseError PowerError) Power
parsePower32 = capture $ do
        n <- first (fmap absurd) word32Le
        if n > upper
            then throwParseError . PowerError $ n
            else maybe (throwParseError . PowerError $ n) pure (toPower (fromIntegral n))
    where
        upper = fromIntegral (maxBound @Word8)
