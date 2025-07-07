{- |
Module: Eeep.Types.Opcode.Target

The @Target@ type.
-}

module Eeep.Types.Opcode.Target (
    -- * Error types.
    TargetError (..),

    -- * Types.
    Target,

    -- ** Constructors.
    toTarget,

    -- * Parsers.
    parseTarget,
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


{- | The t'TargetError' type. -}
newtype TargetError = TargetError Word8
    deriving stock (Eq, Show)


{- | The @Target@ enumeration type. -}
data Target
    = None
    | Self
    | Preset
    | Party
    | Area
    | NotParty
    | CasterGroup
    | TargetGroup
    | NotSelf
    | Original
    deriving stock (Eq, Ord, Enum, Bounded, Ix, Show)


{- | Smart constructor for the 'Target' type.-}
{-# INLINE toTarget #-}
toTarget :: Word8 -> Maybe Target
toTarget n = if m <= fromEnum (maxBound @Target) then Just $ toEnum m else Nothing
    where
        m = fromIntegral n


{- | Parse a t'Target' from a single 'Word8'. -}
parseTarget :: (HasOffset s, ElementOf s ~ Word8) => Parser s (ParseError TargetError) Target
parseTarget = capture $ do
    n <- first (fmap absurd) word8
    maybe (throwParseError $ TargetError n) pure (toTarget n)
