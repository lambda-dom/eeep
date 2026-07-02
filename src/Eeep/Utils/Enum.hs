{- |
Module: Eeep.Utils.Enum

Utilities for enumeration types.
-}

module Eeep.Utils.Enum (
    -- * 'Enum' utilities.
    eitherEnum,
) where


{- | Smart validated constructor for bounded 'Enum' types.-}
{-# INLINE eitherEnum #-}
eitherEnum :: forall a b e . (Integral a, Enum b, Bounded b) => (a -> e) -> a -> Either e b
eitherEnum h n =
    if fromEnum (minBound @b) <= m && m <= fromEnum (maxBound @b)
        then Right $ toEnum m
        else Left (h n)
    where
        m = fromIntegral n
