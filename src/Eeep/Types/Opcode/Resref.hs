{- |
Module: Eeep.Types.Opcode.Resref

The @Resref@ type.
-}

module Eeep.Types.Opcode.Resref (
    -- * Types.
    Resref,

    -- ** Prisms.
    resrefChar,
    resref,
) where

-- Imports.
-- Base.
import Data.Char (isControl, isAscii, chr)
import Data.Word (Word64, Word8)

-- Libraries.
import Optics.Core (Prism', Iso', coercedTo, view, toListOf, each)

-- non-Hackage Libraries.
import Data.Char.Optics (category)
import Data.Bits.Optics (word64BytesLe)


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
    show (Resref n) = "Resref \"" ++ showBytes n ++ "\""
        where
            showBytes :: Word64 -> String
            showBytes = fmap chr . takeWhile (/= 0) . fmap (fromIntegral @Word8 @Int) . unpack

            unpack :: Word64 -> [Word8]
            unpack = toListOf each . view word64BytesLe


{- | Isomorphism for @t'Resref'@. -}
{-# INLINE resref #-}
resref :: Iso' Resref Word64
resref = coercedTo

{- | Prism for @t'Resref'@ characters.


=== __Examples:__
>>> preview resrefChar (fromIntegral . ord $ ' ')
Just 32

>>> preview resrefChar 255
Nothing

>>> preview resrefChar (fromIntegral . ord $ '\n')
Nothing

>>> preview resrefChar (fromIntegral . ord $ '.')
Nothing
-}
{-# INLINE resrefChar #-}
resrefChar :: Prism' Word8 Char
resrefChar = category (p . chr . fromIntegral @Word8 @Int)
    where
        p :: Char -> Bool
        p c = isAscii c && not (isControl c) && c /= '\\' && c /= '/' && c /= '.'
