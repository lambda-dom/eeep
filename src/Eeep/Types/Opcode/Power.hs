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

-- Package.
import Eeep.Typeclasses.Binary (Reader (..))


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

instance (HasOffset s, ElementOf s ~ Word8) => Reader s PowerError Power where
    parser :: Parser s (ParseError PowerError) Power
    parser = capture $ do
        n <- first (fmap absurd) word8
        maybe (throwParseError $ PowerError n) pure (toPower n)



{- | Smart constructor for the t'Power' type.-}
{-# INLINE toPower #-}
toPower :: Word8 -> Maybe Power
toPower n = if n <= 10 then Just $ Power n else Nothing
