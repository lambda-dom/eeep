{- |
Module: Eeep.Types.Opcode.Probability

The @Probability@ type.
-}

module Eeep.Types.Opcode.Probability (
    -- * Types.
    Probability,

    -- ** Constructors.
    toProbability,
) where

-- Imports.
-- Base.
import Data.Word (Word8)


{- | The (non-empty) @Probability@ interval type. -}
data Probability = Probability {
    lower :: {-# UNPACK #-} !Word8,
    upper :: {-# UNPACK #-} !Word8
} deriving stock (Eq, Show)


{- | Construct a @t'Probability'@ pair from a pair of integers. -}
{-# INLINE toProbability #-}
toProbability
    :: Int                              -- ^ Lower bound.
    -> Int                              -- ^ Upper bound.
    -> Maybe Probability
toProbability l u =
    if 0 <= l && l <= u && u <= 100
        then Just $ Probability {lower = fromIntegral l, upper = fromIntegral u}
        else Nothing
