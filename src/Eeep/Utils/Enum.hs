{- |
Module: Eeep.Utils.Enum

Utilities for enumeration types.
-}

module Eeep.Utils.Enum (
    -- * 'Enum' utilities.
    eitherEnum,
) where

-- Imports.
-- non-Hackage libraries.
import Trisagion.Utils.Either ((:+:))


{- | Smart validated constructor for bounded 'Enum' types.-}
{-# INLINE eitherEnum #-}
eitherEnum :: forall a b e . (Integral a, Enum b, Bounded b) => e -> a -> e :+: b
eitherEnum e n =
    if fromEnum (minBound @b) <= m && m <= fromEnum (maxBound @b)
        then Right $ toEnum m
        else Left  $ e
    where
        m = fromIntegral n
