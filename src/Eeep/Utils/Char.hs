{- |
Module: Eeep.Utils.Char

'Char' utilities.
-}

module Eeep.Utils.Char (
    -- * 'Char' utilities.
    char,
) where

-- Imports.
-- Base.
import Data.Char (chr, ord)
import Data.Word (Word8)

-- Libraries.
import Optics.Core (Prism', prism')


{- | Prism for conversion between bytes and characters. -}
{-# INLINE char #-}
char :: Prism' Char Word8
char = prism' construct match
    where
        construct :: Word8 -> Char
        construct = chr . fromIntegral

        match :: Char -> Maybe Word8
        match c = if x <= 0xff then Just $ fromIntegral x else Nothing
            where
                x :: Int
                x = ord c
