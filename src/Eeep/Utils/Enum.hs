{- |
Module: Eeep.Utils.Enum

Utilities for enumeration types.
-}

module Eeep.Utils.Enum (
    -- * 'Enum' utilities.
    enum,
) where

-- Imports.
-- Libraries.
import Optics.Core (Prism', prism')


{- | Prism for converting between integral types and enumerations.

note(s):

    * If there are more values in the enumeration than the integral type holds, than @review 'enum'@
    truncates the result to fit the integral type and 'enum' violates the prism laws. It is the
    responsability of the caller to make sure that that cannot happen.
-}
{-# INLINE enum #-}
enum :: forall a b . (Integral a, Enum b, Bounded b) => Prism' a b
enum = prism' construct match
    where
        construct :: b -> a
        construct = fromIntegral . fromEnum

        match :: a -> Maybe b
        match n =
            if fromEnum (minBound @b) <= m && m <= fromEnum (maxBound @b)
                then Just $ toEnum m
                else Nothing
            where
                m = fromIntegral n
