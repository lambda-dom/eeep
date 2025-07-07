{- |
Module: Eeep.Types.Opcode.Probability

The @Probability@ type.
-}

module Eeep.Types.Opcode.Probability (
    -- * Error types.
    ProbabilityError (..),

    -- * Types.
    Probability,

    -- ** Constructors.
    toProbability,

    -- * Parsers.
    parseProbability8,
    parseProbability16,
) where

-- Imports.
-- Base.
import Data.Bifunctor (Bifunctor (..))
import Data.Void (absurd)
import Data.Word (Word8, Word16)


-- non-Hackage libraries.
import Mono.Typeclasses.MonoFunctor (ElementOf)
import Mono.Typeclasses.MonoFoldable (MonoFoldable)
import Trisagion.Types.ParseError (ParseError)
import Trisagion.Typeclasses.HasOffset (HasOffset)
import Trisagion.Typeclasses.Splittable (Splittable (PrefixOf))
import Trisagion.Parser (Parser)
import Trisagion.Parsers.ParseError (throwParseError, capture)
import Trisagion.Parsers.Word8 (word8, word16Le)


{- | The t'ProbabilityError' type. -}
data ProbabilityError = ProbabilityError !Word16 !Word16
    deriving stock (Eq, Show)


{- | The (non-empty) @Probability@ interval type. -}
data Probability = Probability {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8
    deriving stock (Eq, Show)


{- | Construct a @t'Probability'@ pair from a pair of integers. -}
{-# INLINE toProbability #-}
toProbability
    :: Word8                            -- ^ Lower bound.
    -> Word8                            -- ^ Upper bound.
    -> Maybe Probability
toProbability l u =
    if l <= u && u <= 100
        then Just $ Probability l u
        else Nothing


{- | Parse a t'Probability' from two 'Word8'. -}
parseProbability8
    :: (HasOffset s, ElementOf s ~ Word8)
    => Parser s (ParseError ProbabilityError) Probability
parseProbability8 = capture $ do
        n <- first (fmap absurd) word8
        m <- first (fmap absurd) word8
        maybe
            (throwParseError $ ProbabilityError (fromIntegral n) (fromIntegral m))
            pure
            (toProbability n m)

{- | Parse a t'Probability' from two 'Word16'. -}
parseProbability16
    :: (HasOffset s, Splittable s, MonoFoldable (PrefixOf s), ElementOf (PrefixOf s) ~ Word8)
    => Parser s (ParseError ProbabilityError) Probability
parseProbability16 = capture $ do
        n <- first (fmap absurd) word16Le
        m <- first (fmap absurd) word16Le
        let
            n' = fromIntegral n
            m' = fromIntegral m
        if (n > upper) || (m > upper)
            then throwParseError $ ProbabilityError m n
            else maybe (throwParseError $ ProbabilityError m n) pure (toProbability m' n')
    where
        upper = fromIntegral (maxBound @Word8)
