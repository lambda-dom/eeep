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
    parsePower,
) where

-- Imports.
-- Base.
import Data.Bifunctor (Bifunctor(..))
import Data.Ix (Ix)
import Data.Void (absurd)
import Data.Word (Word8)

-- non-Hackage libraries.
import Mono.Typeclasses.MonoFunctor (ElementOf)
import Trisagion.Types.ParseError (ParseError)
import Trisagion.Typeclasses.HasOffset (HasOffset)
import Trisagion.Parser (Parser)
import Trisagion.Parsers.ParseError (throwParseError, capture)
import Trisagion.Parsers.Word8 (word8)


{- | The t'PowerError' type. -}
newtype PowerError = PowerError Word8
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
parsePower :: (HasOffset s, ElementOf s ~ Word8) => Parser s (ParseError PowerError) Power
parsePower = capture $ do
    n <- first (fmap absurd) word8
    maybe (throwParseError $ PowerError n) pure (toPower n)


