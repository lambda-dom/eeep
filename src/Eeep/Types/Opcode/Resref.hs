{- |
Module: Eeep.Types.Opcode.Resref

The @Resref@ type.
-}

module Eeep.Types.Opcode.Resref (
    -- * Error types.
    ResrefError (..),

    -- * Types.
    Resref,

    -- ** Predicates.
    isResrefChar,

    -- ** Prisms.
    resref,

    -- ** Parsers.
    decode,
) where

-- Imports.
-- Base.
import Data.Char (isControl, isAscii, chr)
import Data.Word (Word8)

-- Libraries.
import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as Bytes (length, all, takeWhile)

-- non-Hackage libraries.
import Mono.Typeclasses.MonoFoldable (MonoFoldable)
import Optics.Core (Prism', prism', preview)
import Trisagion.Utils.Either ((:+:), annotate)
import Trisagion.Parser (Parser)
import Trisagion.Parsers.Source (InputError)
import Trisagion.Typeclasses.Parsers.Binary (Binary (..))
import Trisagion.Parsers.Combinators (validate)
import Trisagion.Parsers.Split (takeExact)


{- | The @ResrefError@ type. -}
newtype ResrefError = ResrefError ShortByteString
    deriving stock (Eq, Ord, Show)


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


{- | Decode a t'Resref'. -}
{-# INLINE decode #-}
decode :: (MonoFoldable Word8 b, Binary b s) => Parser s (ResrefError :+: InputError) Resref
decode = validate (annotate ResrefError (preview resref)) (shortBytes $ takeExact 8)
