{- |
Module: Eeep.Types.Opcode.Resref

The @Resref@ type.
-}

module Eeep.Types.Opcode.Resref (
    -- * Types.
    Resref,

    -- ** Validators.
    isValid,
) where

-- Imports.
-- Base.
import Data.Char (isControl, isAscii)
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
    {-# INLINEABLE show #-}
    show :: Resref -> String
    show (Resref n) = "Resref '" ++ showBytes n ++ "'"
        where
            showBytes :: Word64 -> String
            showBytes = fmap (review char) . takeWhile (/= 0) . unpack


{- | Validate a t'Char' for a resource reference.

=== __Examples:__

>>> isValid (fromIntegral . ord $ ' ')
Just 32

>>> isValid 255
Nothing

>>> isValid (fromIntegral . ord $ '\n')
Nothing

>>> isValid (fromIntegral . ord $ '.')
Nothing
-}
{-# INLINE isValid #-}
isValid :: Word8 -> Maybe Word8
isValid n = if v (review char n) then Just n else Nothing
    where
        v :: Char -> Bool
        v d = isAscii d && not (isControl d) && d /= '\\' && d /= '/' && d /= '.'
