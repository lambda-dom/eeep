{- |
Module: Eeep.Types.Opcode.Resref

The @Resref@ type.
-}

module Eeep.Types.Opcode.Resref (
    -- * Types.
    Resref,

    -- ** Predicates.
    isResrefChar,

    -- ** Prisms.
    resrefChar,
) where

-- Imports.
-- Base.
import Data.Char (isControl, isAscii, chr)
import Data.Word (Word8)

-- Libraries.
import Data.ByteString.Short (ShortByteString)
import Optics.Core (Prism')

-- non-Hackage Libraries.
import Data.Char.Optics (category)


{- | The t'Resref' type for resource references.

note(s):

    * The 'Ord' instance has no semantic significance and is here merely to be able to use
    t'Resref' values as map keys, etc.
-}
newtype Resref = Resref ShortByteString
    deriving stock (Eq, Ord, Show)


{- | Validate a 'Char' as a valid t'Resref' char.

=== __Examples:__
>>> isResrefChar ' '
True

>>> isResrefChar (chr 255)
False

>>>  isResrefChar '\n'
False

>>> isResrefChar '.'
False
-}
isResrefChar :: Char -> Bool
isResrefChar c = isAscii c && not (isControl c) && c /= '\\' && c /= '/' && c /= '.' && c /= ':'


{- | Prism for @t'Resref'@ characters. -}
{-# INLINE resrefChar #-}
resrefChar :: Prism' Word8 Char
resrefChar = category (isResrefChar . chr . fromIntegral @Word8 @Int)
