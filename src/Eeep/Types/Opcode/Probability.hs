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
import Data.Word (Word8, Word16)


{- | The (non-empty) @Probability@ interval type. -}
data Probability = Probability {
    _lower :: {-# UNPACK #-} !Word8,
    _upper :: {-# UNPACK #-} !Word8
    } deriving stock (Eq, Show)


{- | Construct a @t'Probability'@ pair from a pair of integers. -}
{-# INLINE toProbability #-}
toProbability
    :: Word16                              -- ^ Lower bound.
    -> Word16                              -- ^ Upper bound.
    -> Maybe Probability
toProbability l u =
    if 0 <= l && l <= u && u <= 100
        then Just $ Probability {_lower = fromIntegral l, _upper = fromIntegral u}
        else Nothing
