{-# LANGUAGE NoFieldSelectors #-}

{- |
Module: Eeep.Types.Opcode.APR

The @APR@ type.
-}

module Eeep.Types.Opcode.APR (
    -- * The t'APR' @Modifier@ type.
    Modifier,

    -- ** Prisms.
    modifier,

    -- * The t'APR' type.
    APR (..),

    -- ** Prisms.
    apr,

) where

-- Imports.
-- Base.
import Data.Int (Int32)
import Data.Ix (Ix)

-- Libraries.
import Optics.Core (Prism', prism', review, view, preview)

-- Package.
import Eeep.Utils.Enum (enum)
import Eeep.Types.Shared.Percentage (Percentage, percentage)
import Data.Word (Word32)


{- | The APR @Modifier@ type.

A refinement type containing the values in the interval @[-10 .. 10]@.
-}
newtype Modifier = Modifier Int32
    deriving stock (Eq, Ord, Ix, Show)
    deriving newtype Enum

-- Instances.
instance Bounded Modifier where
    {-# INLINE minBound #-}
    minBound :: Modifier
    minBound = Modifier (-10)

    {-# INLINE maxBound #-}
    maxBound :: Modifier
    maxBound = Modifier 10


{- | Prism for the t'Modifier' type. -}
{-# INLINABLE modifier #-}
modifier :: Prism' Int32 Modifier
modifier = enum


{- | The @APR@ coproduct type. -}
data APR
    = Cumulative !Modifier
    | Flat       !Modifier
    | Percentage !Percentage
    | Final      !Modifier
    deriving stock (Eq, Ord, Show)


{- | The prism for the t'APR' type. -}
{-# INLINABLE apr #-}
apr :: Prism' (Word32, Word32) APR
apr = prism' construct match
    where
        construct :: APR -> (Word32, Word32)
        construct (Cumulative m) = (0, fromIntegral $ review modifier m)
        construct (Flat m)       = (1, fromIntegral $ review modifier m)
        construct (Percentage p) = (2, view percentage p)
        construct (Final m)      = (3, fromIntegral $ review modifier m)

        match :: (Word32, Word32) -> Maybe APR
        match (0, n) = fmap Cumulative (preview modifier (fromIntegral n))
        match (1, n) = fmap Flat (preview modifier (fromIntegral n))
        match (2, n) = Just $ Percentage (review percentage n)
        match (3, n) = fmap Final (preview modifier (fromIntegral n))
        match (_, _) = Nothing
