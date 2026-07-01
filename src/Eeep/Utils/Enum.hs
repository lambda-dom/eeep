{- |
Module: Eeep.Utils.Enum

Utilities for enumeration types.
-}

module Eeep.Utils.Enum (
    -- * 'Enum' utilities.
    maybeEnum,
) where


{- | Smart constructor for bounded 'Enum' types.-}
{-# INLINE maybeEnum #-}
maybeEnum :: forall a b . (Integral a, Enum b, Bounded b) => a -> Maybe b
maybeEnum n =
    if fromEnum (minBound @b) <= m && m <= fromEnum (maxBound @b)
        then Just $ toEnum m
        else Nothing
    where
        m = fromIntegral n
