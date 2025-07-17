{- |
Module: Eeep.Types.Opcode.Probability

The @Probability@ type.
-}

module Eeep.Types.Opcode.Probability (
    -- * Error types.
    ProbabilityError (..),

    -- * Types.
    Probability,

    -- ** Prisms.
    probability,

    -- * Parsers.
    decodeProbability8,
    decodeProbability16,

    -- * Serializers.
    encodeProbability8,
    encodeProbability16,
) where

-- Imports.
-- Base.
import Data.Bifunctor (Bifunctor (..))
import Data.Functor.Contravariant (Contravariant (..))
import Data.Void (absurd)
import Data.Word (Word8, Word16)

-- Libraries.
import Data.Functor.Contravariant.Divisible (Divisible (..))
import Optics.Core (Prism', prism', preview, review)

-- non-Hackage libraries.
import Mono.Typeclasses.MonoFunctor (ElementOf)
import Mono.Typeclasses.MonoFoldable (MonoFoldable)
import Trisagion.Types.ParseError (ParseError)
import Trisagion.Typeclasses.HasOffset (HasOffset)
import Trisagion.Typeclasses.Splittable (Splittable (PrefixOf))
import Trisagion.Typeclasses.Binary (Binary)
import Trisagion.Parser (Parser)
import Trisagion.Parsers.ParseError (throwParseError, capture)
import Trisagion.Parsers.Word8 (word8, word16Le)
import Trisagion.Serializer (Serializer)
import qualified Trisagion.Serializers.Binary as Binary (word16Le, word8)


{- | The t'ProbabilityError' type. -}
data ProbabilityError = ProbabilityError !Word16 !Word16
    deriving stock (Eq, Show)


{- | The (non-empty) @Probability@ interval type. -}
data Probability = Probability {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8
    deriving stock (Eq, Show)


{- | Prism for a t'Probability' interval with lower and upper bounds @(n, m)@. -}
{-# INLINE probability #-}
probability :: Prism' (Word8, Word8) Probability
probability = prism' construct match
    where
        construct :: Probability -> (Word8, Word8)
        construct (Probability lower upper) = (lower, upper)

        match :: (Word8, Word8) -> Maybe Probability
        match (lower, upper) =
            if lower <= upper && upper <= 100 then Just $ Probability lower upper else Nothing


{- | Parse a t'Probability' from two 'Word8'. -}
decodeProbability8
    :: (HasOffset s, ElementOf s ~ Word8)
    => Parser s (ParseError ProbabilityError) Probability
decodeProbability8 = capture $ do
        n <- first (fmap absurd) word8
        m <- first (fmap absurd) word8
        maybe
            (throwParseError $ ProbabilityError (fromIntegral n) (fromIntegral m))
            pure
            (preview probability (n, m))

{- | Parse a t'Probability' from two 'Word16'. -}
decodeProbability16
    :: (HasOffset s, Splittable s, MonoFoldable (PrefixOf s), ElementOf (PrefixOf s) ~ Word8)
    => Parser s (ParseError ProbabilityError) Probability
decodeProbability16 = capture $ do
        n <- first (fmap absurd) word16Le
        m <- first (fmap absurd) word16Le
        let
            n' = fromIntegral n
            m' = fromIntegral m
        if (n > upper) || (m > upper)
            then throwParseError $ ProbabilityError m n
            else maybe (throwParseError $ ProbabilityError m n) pure (preview probability (m', n'))
    where
        upper = fromIntegral (maxBound @Word8)

{- | Encode a t'Probability' into two 'Word8'. -}
encodeProbability8 :: Binary m => Serializer m Probability
encodeProbability8 =
    contramap
        (review probability)
        (divide id Binary.word8 Binary.word8)

{- | Encode a t'Probability' into two 'Word16'. -}
encodeProbability16 :: Binary m => Serializer m Probability
encodeProbability16 =
    contramap
        (bimap fromIntegral fromIntegral . review probability)
        (divide id Binary.word16Le Binary.word16Le)
