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
    decodeTarget8,
    decodeTarget32,

    -- * Serializers.
    encodeTarget8,
    encodeTarget32,
) where

-- Imports.
-- Base.
import Data.Bifunctor (Bifunctor (..))
import Data.Functor.Contravariant (Contravariant(..))
import Data.Ix (Ix)
import Data.Void (absurd)
import Data.Word (Word8, Word32)

-- non-Hackage libraries.
import Mono.Typeclasses.MonoFunctor (ElementOf)
import Mono.Typeclasses.MonoFoldable (MonoFoldable)
import Trisagion.Types.ParseError (ParseError)
import Trisagion.Typeclasses.HasOffset (HasOffset)
import Trisagion.Typeclasses.Splittable (Splittable (PrefixOf))
import Trisagion.Typeclasses.Binary (Binary)
import Trisagion.Parser (Parser)
import Trisagion.Parsers.ParseError (throwParseError, capture)
import Trisagion.Parsers.Word8 (word8, word32Le)
import Trisagion.Serializer (Serializer)
import qualified Trisagion.Serializers.Binary as Binary (word32Le, word8)


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


{- | Decode a t'Target' from a single 'Word8'. -}
decodeTarget8 :: (HasOffset s, ElementOf s ~ Word8) => Parser s (ParseError TargetError) Target
decodeTarget8 = capture $ do
    n <- first (fmap absurd) word8
    maybe (throwParseError . TargetError . fromIntegral $ n) pure (toTarget n)

{- | Decode a t'Target' from a single 'Word32'. -}
decodeTarget32
    :: (HasOffset s, Splittable s, MonoFoldable (PrefixOf s), ElementOf (PrefixOf s) ~ Word8)
    => Parser s (ParseError TargetError) Target
decodeTarget32 = capture $ do
        n <- first (fmap absurd) word32Le
        if n > upper
            then throwParseError . TargetError $ n
            else maybe (throwParseError . TargetError $ n) pure (toTarget (fromIntegral n))
    where
        upper = fromIntegral (maxBound @Word8)

{- | Encode an t'Target' into a 'Word8'. -}
encodeTarget8 :: Binary m => Serializer m Target
encodeTarget8 = contramap (fromIntegral . fromEnum) Binary.word8

{- | Encode a t'Target' into a 'Word32'. -}
encodeTarget32 :: Binary m => Serializer m Target
encodeTarget32 = contramap (fromIntegral . fromEnum) Binary.word32Le
