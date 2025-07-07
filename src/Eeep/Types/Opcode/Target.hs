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
    parseTarget8,

    -- * Types.
    parseTarget32,
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


{- | The t'TargetError' type. -}
newtype TargetError = TargetError Word32
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
parseTarget8 :: (HasOffset s, ElementOf s ~ Word8) => Parser s (ParseError TargetError) Target
parseTarget8 = capture $ do
    n <- first (fmap absurd) word8
    maybe (throwParseError . TargetError . fromIntegral $ n) pure (toTarget n)

{- | Parse a t'Target' from a single 'Word32'. -}
parseTarget32
    :: (HasOffset s, Splittable s, MonoFoldable (PrefixOf s), ElementOf (PrefixOf s) ~ Word8)
    => Parser s (ParseError TargetError) Target
parseTarget32 = capture $ do
        n <- first (fmap absurd) word32Le
        if n > upper
            then throwParseError . TargetError $ n
            else maybe (throwParseError . TargetError $ n) pure (toTarget (fromIntegral n))
    where
        upper = fromIntegral (maxBound @Word8)
