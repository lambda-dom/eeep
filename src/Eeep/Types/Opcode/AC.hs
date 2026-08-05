{-# LANGUAGE NoFieldSelectors #-}

{- |
Module: Eeep.Types.Opcode.AC

The @AC@ type.
-}

module Eeep.Types.Opcode.AC (
    -- * The t'AC' @Modifier@ type.
    Modifier,

    -- ** Prisms.
    modifier,

    -- * The @AC@ type.
    AC (..),

    -- ** Prisms.
    ac,
) where

-- Imports.
-- Base.
import Data.Int (Int32)
import Data.Ix (Ix)
import Data.Word (Word32)

-- Libraries.
import Optics.Core (Prism', prism', review, preview)

-- Package.
import Eeep.Utils.Enum (enum)


{- | The AC @Modifier@ type.

A refinement type containing the values in the interval @[-20 .. 20]@.
-}
newtype Modifier = Modifier Int32
    deriving stock (Eq, Ord, Ix, Show)
    deriving newtype Enum

-- Instances.
instance Bounded Modifier where
    {-# INLINE minBound #-}
    minBound :: Modifier
    minBound = Modifier (-20)

    {-# INLINE maxBound #-}
    maxBound :: Modifier
    maxBound = Modifier 20


{- | Prism for the t'Modifier' type. -}
{-# INLINABLE modifier #-}
modifier :: Prism' Int32 Modifier
modifier = enum


{- | The t'AC' type. -}
data AC
    = All      !Modifier
    | Crushing !Modifier
    | Missile  !Modifier
    | Piercing !Modifier
    | Slashing !Modifier
    | Base     !Modifier
    deriving stock (Eq, Ord, Show)


{- | The prism for the t'AC' type. -}
{-# INLINABLE ac #-}
ac :: Prism' (Word32, Word32) AC
ac = prism' construct match
    where
        construct :: AC -> (Word32, Word32)
        construct (All m)      = (0, fromIntegral $ review modifier m)
        construct (Crushing m) = (1, fromIntegral $ review modifier m)
        construct (Missile m)  = (2, fromIntegral $ review modifier m)
        construct (Piercing m) = (4, fromIntegral $ review modifier m)
        construct (Slashing m) = (8, fromIntegral $ review modifier m)
        construct (Base m)     = (16,fromIntegral $ review modifier m)

        match :: (Word32, Word32) -> Maybe AC
        match (0, n)  = fmap All $ preview modifier (fromIntegral n)
        match (1, n)  = fmap Crushing $ preview modifier (fromIntegral n)
        match (2, n)  = fmap Missile $ preview modifier (fromIntegral n)
        match (4, n)  = fmap Piercing $ preview modifier (fromIntegral n)
        match (8, n)  = fmap Slashing $ preview modifier (fromIntegral n)
        match (16, n) = fmap Base $ preview modifier (fromIntegral n)
        match (_, _)  = Nothing
