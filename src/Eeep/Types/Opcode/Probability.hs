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

    -- ** Parsers and serializers.
    encodeProbability,
    decodeProbability,

    -- ** Functions.
    isEmpty,
    isElem,
    toList,
) where

-- Imports.
-- Base.
import Data.Bifunctor (Bifunctor (..))
import Data.Functor.Contravariant (Contravariant (..))
import Data.Word (Word8)

-- Libraries.
import Data.Functor.Contravariant.Divisible (divided)
import Control.Monad.Except (MonadError (..))
import Optics.Core (Prism', prism', preview, review)

-- non-Hackage libraries.
import Trisagion.Utils.Either ((:+:))
import Trisagion.Typeclasses.Source (Source)
import Trisagion.Parser (Parser)
import Trisagion.Parsers.Source (InputError, one)
import Trisagion.Serializer (Serializer)
import Trisagion.Serializers.Binary (Binary, word8)


{- | The t'ProbabilityError' type. -}
data ProbabilityError = ProbabilityError !Word8 !Word8
    deriving stock (Eq, Show)


{- | The @Probability@ interval type.

A _probability interval_ is a closed-open interval @[l, u[@ with @(0 <= l < 100) && (0 <= u <= 100)@.
If @l >= u@ then the interval is empty.
-}
data Probability = Probability !Word8 !Word8
    deriving stock (Eq, Show)


{- | Prism for a t'Probability' interval. -}
{-# INLINE probability #-}
probability :: Prism' (Word8, Word8) Probability
probability = prism' construct match
    where
        construct :: Probability -> (Word8, Word8)
        construct (Probability l u) = (l, u)

        match :: (Word8, Word8) -> Maybe Probability
        match (l, u) =
            -- Can have lower >= upper in which case interval is empty.
            if l < 100 && u <= 100 then Just $ Probability l u else Nothing


{- | Default parser for t'Probability'. -}
{-# INLINE encodeProbability #-}
encodeProbability :: Source Word8 s => Parser s (ProbabilityError :+: InputError) Probability
encodeProbability = do
    l <- first Right one
    u <- first Right one
    maybe
        (throwError . Left $ ProbabilityError l u)
        pure
        (preview probability (l, u))

{- | Default serializer for t'Probability'. -}
{-# INLINE decodeProbability #-}
decodeProbability :: Binary b s => Serializer s Probability
decodeProbability = contramap (review probability) (divided word8 word8)


{- | Return 'True' if t'Probability' interval is empty. -}
{-# INLINE isEmpty #-}
isEmpty :: Probability -> Bool
isEmpty (Probability l u) = l >= u

{- | Return 'True' if @elem@ is an element in the t'Probability' interval. -}
{-# INLINE isElem #-}
isElem :: Word8 -> Probability -> Bool
isElem n (Probability l u) = l <= n && n < u

{- | Return the list of elements of the t'Probability' interval. -}
{-# INLINE toList #-}
toList :: Probability -> [Word8]
toList (Probability l u) = enumFromTo l u
