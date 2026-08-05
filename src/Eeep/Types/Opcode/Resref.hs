{- |
Module: Eeep.Types.Opcode.Resref

The @Resref@ type.
-}

module Eeep.Types.Opcode.Resref (
    -- * Types.
    Resref,

    -- ** Predicates.
    isValid,
) where

-- Imports.
-- Base.
import Data.Char (isAscii)
import Data.Word (Word64, Word8)

-- Libraries.
import Optics.Core (review)

-- non-Hackage libraries.
import Trisagion.Utils.Bits (unpack)

-- Package.
import Eeep.Utils.Char (char)


{- | The t'Resref' type for resource references.

note(s):

    * The 'Ord' and 'Bounded' instances have no semantic significance and are here merely to be able
    to use t'Resref' values as map keys, etc.
-}
newtype Resref = Resref Word64
    deriving stock (Eq, Ord, Bounded)


-- Instances.
instance Show Resref where
    {-# INLINABLE show #-}
    show :: Resref -> String
    show (Resref n) = "Resref '" ++ showBytes n ++ "'"
        where
            showBytes :: Word64 -> String
            showBytes = fmap (review char) . takeWhile (/= 0) . unpack


{- | Validate a t'Char' for a resource reference.

A character is valid iff it is an ascii character that is not any of the file path special
characters (slash, dot, etc.).

=== __Examples:__

>>> isValid (fromIntegral . ord $ ' ')
True

>>> isValid 255
False

>>> isValid (fromIntegral . ord $ '.')
False
-}
{-# INLINABLE isValid #-}
isValid :: Word8 -> Bool
isValid n = isAscii c && ('\\' /= c) && ('/' /= c) && ('.' /= c) && (':' /= c)
    where
        c :: Char
        c = review char n
