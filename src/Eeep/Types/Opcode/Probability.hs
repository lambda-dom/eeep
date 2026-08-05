{- |
Module: Eeep.Types.Opcode.Probability

The @Probability@ type.
-}

module Eeep.Types.Opcode.Probability (
    -- * Types.
    Probability,

    -- ** Prisms.
    probability,

    -- ** Getters.
    lower,
    upper,

    -- ** Functions.
    isEmpty,
    isElem,
    toList,
) where

-- Imports.
-- Base.
import Data.Word (Word8)

-- Libraries.
import Optics.Core (Prism', prism', review)


{- | The @Probability@ interval type.

A _probability interval_ is a closed interval @[l, u]@ with @0 <= l, u <= 100@. If @l > u@ then
the interval is empty.
-}
data Probability = Probability !Word8 !Word8
    deriving stock (Eq, Show)


{- | Prism for a t'Probability' interval. -}
{-# INLINABLE probability #-}
probability :: Prism' (Word8, Word8) Probability
probability = prism' construct match
    where
        construct :: Probability -> (Word8, Word8)
        construct (Probability l u) = (l, u)

        match :: (Word8, Word8) -> Maybe Probability
        match (l, u) =
            -- Can have lower > upper in which case interval is empty.
            if l <= 100 && u <= 100 then Just $ Probability l u else Nothing


{- | Return the lower bound of the t'Probability' interval. -}
{-# INLINE lower #-}
lower :: Probability -> Word8
lower = fst . review probability

{- | Return the upper bound of the t'Probability' interval. -}
{-# INLINE upper #-}
upper :: Probability -> Word8
upper = snd . review probability


{- | Return 'True' if the t'Probability' interval is empty. -}
{-# INLINE isEmpty #-}
isEmpty :: Probability -> Bool
isEmpty (Probability l u) = l > u

{- | Return 'True' if @elem@ is an element in the t'Probability' interval. -}
{-# INLINE isElem #-}
isElem :: Word8 -> Probability -> Bool
isElem n (Probability l u) = l <= n && n <= u

{- | Return the list of elements of the t'Probability' interval. -}
{-# INLINE toList #-}
toList :: Probability -> [Word8]
toList (Probability l u) = enumFromTo l u
