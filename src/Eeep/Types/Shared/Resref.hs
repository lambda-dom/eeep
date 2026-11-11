{- |
Module: Eeep.Types.Shared.Resref

The @Resref@ type.
-}

module Eeep.Types.Shared.Resref (
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
import Data.Char (isControl, isAscii)
import Data.Functor.Contravariant (Contravariant (..))
import Data.Word (Word8)

-- Libraries.
import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as Short (length, takeWhile, replicate)

-- non-Hackage libraries.
import Data.MonoFunctor (MonoFunctor)
import Data.MonoPointed (MonoPointed)
import Data.MonoFoldable (MonoFoldable (..))
import Trisagion.Parser (Parser)
import Trisagion.Parsers.IStream (InputError)
import Trisagion.Parsers.Split (takeExact)
import qualified Trisagion.Parsers.Binary as Parsers (Binary (..))
import Trisagion.Serializer (Serializer)
import qualified Trisagion.Serializers.Binary as Serializers (Binary (..))


{- | The t'Resref' type for resource references.

The type is a wrapper around a 'ShortByteString' of length @< 8@. These represent the basename of
files, so even though no validation is done (to able to apply all 'ShortByteString' functions) it
should not contain characters like @\\@ or @.@ that are used as path separators -- see
'isResrefChar' below.

note(s):

    * The 'Ord' instance has no semantic significance and is here merely to be able to use
    t'Resref' values as map keys, etc.
-}
newtype Resref = Resref ShortByteString
    deriving stock (Eq, Ord)
    deriving newtype (Show, MonoFunctor Word8, MonoPointed Word8)


-- Instances.
-- Note: manual instance to shut up GHC.
instance MonoFoldable Word8 Resref where
    {-# INLINE monotoList #-}
    monotoList :: Resref -> [Word8]
    monotoList (Resref xs) = monotoList xs

    {-# INLINE monofoldMap #-}
    monofoldMap :: Monoid m => (Word8 -> m) -> Resref -> m
    monofoldMap f (Resref xs) = monofoldMap f xs

    {-# INLINE monofoldr #-}
    monofoldr :: (Word8 -> b -> b) -> b -> Resref -> b
    monofoldr f x (Resref xs) = monofoldr f x xs

    {-# INLINE monofoldl #-}
    monofoldl :: (b -> Word8 -> b) -> b -> Resref -> b
    monofoldl f x (Resref xs) = monofoldl f x xs

    {-# INLINE mononull #-}
    mononull :: Resref -> Bool
    mononull (Resref xs) = mononull xs

    {-# INLINE monolength #-}
    monolength :: Resref -> Int
    monolength (Resref xs) = monolength xs

    {-# INLINE monoelem #-}
    monoelem :: Word8 -> Resref -> Bool
    monoelem x (Resref xs) = monoelem x xs


{- | Smart constructor for t'Resref'.

note(s):

    * The constructor performs normalization so it does not form a prism with 'toShort'.
-}
fromShort :: ShortByteString -> Maybe Resref
fromShort xs =
    if 8 < Short.length xs
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
decode :: (MonoFoldable Word8 b, Parsers.Binary b s) => Parser s InputError Resref
decode = fmap Resref (Parsers.shortBytes $ takeExact 8)

{- | Encode a t'Resref'. -}
{-# INLINE encode #-}
encode :: Serializers.Binary s => Serializer s Resref
encode = contramap (padN 8 . toShort) Serializers.shortBytes


{- | Pad a 'ShortByteString' on the right with @n@ zeros. -}
padN :: Int -> ShortByteString -> ShortByteString
padN m xs =
    let l = m - Short.length xs in
        if l <= 0
        then xs
        else xs <> Short.replicate l 0
