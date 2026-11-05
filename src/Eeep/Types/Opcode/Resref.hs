{- |
Module: Eeep.Types.Opcode.Resref

The @Resref@ type.
-}

module Eeep.Types.Opcode.Resref (
    -- * Error types.
    Error (..),

    -- * Types.
    Resref,

    -- ** Smart constructors and eliminators.
    fromShort,
    toShort,

    -- ** Predicates.
    isResrefChar,

    -- ** Parsers and serializers.
    decode,
    encode,

    -- * Utilities.
    padN,
) where

-- Imports.
-- Base.
import Data.Char (isControl, isAscii, chr)
import Data.Functor.Contravariant (Contravariant (..))
import Data.Word (Word8)

-- Libraries.
import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as Short (length, all, takeWhile, replicate)

-- non-Hackage libraries.
import Mono.Typeclasses.MonoFoldable (MonoFoldable)
import Trisagion.Utils.Either ((:+:), annotate)
import Trisagion.Parser (Parser)
import Trisagion.Parsers.Source (InputError)
import qualified Trisagion.Typeclasses.Parsers.Binary as Parsers (Binary (..))
import Trisagion.Parsers.Combinators (validate)
import Trisagion.Parsers.Split (takeExact)
import qualified Trisagion.Typeclasses.Serializers.Binary as Serializers (Binary (..))
import Trisagion.Serializer (Serializer)


{- | The t'Resref' @Error@ type. -}
newtype Error = Error ShortByteString
    deriving stock (Eq, Ord, Show)


{- | The t'Resref' type for resource references.

note(s):

    * The 'Ord' instance has no semantic significance and is here merely to be able to use
    t'Resref' values as map keys, etc.
-}
newtype Resref = Resref ShortByteString
    deriving stock (Eq, Ord, Show)


{- | Smart constructor for t'Resref'.

note(s):

    * The constructor performs normalization so it is not part of a 'Optics.Core.Prism'.
-}
fromShort :: ShortByteString -> Maybe Resref
fromShort xs =
    if 8 < Short.length xs && Short.all (isResrefChar . chr . fromIntegral @Word8 @Int) xs
    then Nothing
    else Just . Resref $ Short.takeWhile (/= 0) xs

{- | Eliminator for t'Resref'. -}
toShort :: Resref -> ShortByteString
toShort (Resref xs) = xs


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
decode :: (MonoFoldable Word8 b, Parsers.Binary b s) => Parser s (Error :+: InputError) Resref
decode = validate (annotate Error fromShort) (Parsers.shortBytes $ takeExact 8)

{- | Encode a t'Resref'. -}
{-# INLINE encode #-}
encode :: Serializers.Binary b s => Serializer s Resref
encode = contramap (padN 8 . toShort) Serializers.shortBytes


{- | Pad a 'ShortByteString' on the right with @n@ zeros. -}
padN :: Int -> ShortByteString -> ShortByteString
padN m xs =
    let l = m - Short.length xs in
        if l <= 0
        then xs
        else xs <> Short.replicate l 0
