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
    resref,
) where

-- Imports.
-- Base.
import Data.Char (isControl, isAscii, chr)
import Data.Word (Word8)

-- Libraries.
import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as Bytes (length, all, takeWhile)
import Optics.Core (Prism', prism')


{- | The t'Resref' type for resource references.

note(s):

    * The 'Ord' instance has no semantic significance and is here merely to be able to use
    t'Resref' values as map keys, etc.
-}
newtype Resref = Resref ShortByteString
    deriving stock (Eq, Ord, Show)


{- | Prism for t'Resref'. -}
resref :: Prism' ShortByteString Resref
resref = prism' construct matched
    where
        construct :: Resref -> ShortByteString
        construct (Resref xs) = xs 

        matched :: ShortByteString -> Maybe Resref
        matched xs =
            if 8 < Bytes.length xs && Bytes.all (isResrefChar . chr . fromIntegral @Word8 @Int) xs
            then Nothing
            else Just . Resref $ Bytes.takeWhile (/= 0) xs


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
