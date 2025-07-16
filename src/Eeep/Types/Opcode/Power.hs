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
    decodePower8,
    decodePower32,

    -- * Types.
    encodePower8,
    encodePower32,
) where

-- Imports.
-- Base.
import Data.Bifunctor (Bifunctor (..))
import Data.Functor.Contravariant (Contravariant (..))
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


{- | Decode a t'Power' from a single 'Word8'. -}
decodePower8 :: (HasOffset s, ElementOf s ~ Word8) => Parser s (ParseError PowerError) Power
decodePower8 = capture $ do
    n <- first (fmap absurd) word8
    maybe (throwParseError . PowerError . fromIntegral $ n) pure (toPower n)

{- | Decode a t'Power' from a single 'Word32'. -}
decodePower32
    :: (HasOffset s, Splittable s, MonoFoldable (PrefixOf s), ElementOf (PrefixOf s) ~ Word8)
    => Parser s (ParseError PowerError) Power
decodePower32 = capture $ do
        n <- first (fmap absurd) word32Le
        if n > upper
            then throwParseError . PowerError $ n
            else maybe (throwParseError . PowerError $ n) pure (toPower (fromIntegral n))
    where
        upper = fromIntegral (maxBound @Word8)

{- | Encode a t'Power' into a 'Word8'. -}
encodePower8 :: Binary m => Serializer m Power
encodePower8 = contramap unwrap Binary.word8
    where
        unwrap :: Power -> Word8
        unwrap (Power n) = n

{- | Encode a t'Power' into a 'Word32'. -}
encodePower32 :: Binary m => Serializer m Power
encodePower32 = contramap (fromIntegral . unwrap) Binary.word32Le
    where
        unwrap :: Power -> Word8
        unwrap (Power n) = n
