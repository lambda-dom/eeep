{-# LANGUAGE UndecidableInstances #-}

{- |
Module: Eeep.Types.Opcode.Resref

The @Resref@ type.
-}

module Eeep.Types.Opcode.Resref (
    -- * Error types.
    ResrefError (..),

    -- * Types.
    Resref,

    -- ** Validators.
    isValid,
) where

-- Imports.
-- Base.
import Data.Bifunctor (Bifunctor (..))
import Data.Char (isControl, isAscii)
import Data.Functor.Contravariant (Contravariant (..))
import Data.Ix (Ix)
import Data.Word (Word64, Word8)

-- Libraries.
import Control.Monad.Except (MonadError (..))
import Optics.Core (review)

-- non-Hackage libraries.
import Mono.Typeclasses.MonoFoldable (MonoFoldable (..))
import Trisagion.Utils.Either ((:+:))
import Trisagion.Utils.Bits (unpack, pack)
import Trisagion.Typeclasses.Split (Split)
import Trisagion.Typeclasses.Source (Source)
import Trisagion.Typeclasses.Sink (Sink)
import Trisagion.Parser (Parser)
import Trisagion.Parsers.Source (InputError)
import Trisagion.Parsers.Split (takeExact)
import Trisagion.Serializer (Serializer)
import qualified Trisagion.Serializers.Binary as Serializers (Binary, word64Le)

-- Package.
import Eeep.Utils.Char (char)
import Eeep.Typeclasses.Binary (Writer (..), Reader (..))


{- | The t'ResrefError' type. -}
newtype ResrefError = ResrefError Char
    deriving stock (Eq, Ord, Bounded, Ix, Show)
    deriving newtype Enum


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

instance (Source Word8 s, Split Word8 b s, MonoFoldable Word8 b) => Reader s (ResrefError :+: InputError) Resref where
    {-# INLINE parser #-}
    parser :: Parser s (ResrefError :+: InputError) Resref
    parser = do
        xs <- first Right (takeExact 8)
        -- Normalize sequence of bytes by dropping everything to the right of the first 0.
        case mapM isValid (takeWhile (/= 0) $ monotoList xs) of
            Left e   -> throwError $ Left e
            Right ys -> pure . Resref . pack $ ys

instance (Sink Word8 b s, Serializers.Binary b s) => Writer b s Resref where
    {-# INLINE serializer #-}
    serializer :: Serializer s Resref
    serializer = contramap unwrap Serializers.word64Le
        where
            -- Assumes normalized resref.
            unwrap :: Resref -> Word64
            unwrap (Resref n) = n


{- | Validate a 'Char' for a resource reference.

=== __Examples:__

>>> isValid (fromIntegral . ord $ ' ')
Right 32

>>> isValid 255
Left (ResrefError '\255')

>>> isValid (fromIntegral . ord $ '\n')
Left (ResrefError '\n')

>>> isValid (fromIntegral . ord $ '.')
Left (ResrefError '.')
-}
{-# INLINE isValid #-}
isValid :: Word8 -> ResrefError :+: Word8
isValid n = if v c then Right n else Left $ ResrefError c
    where
        c :: Char
        c = review char n

        v :: Char -> Bool
        v d = isAscii d && not (isControl d) && d /= '\\' && d /= '/' && d /= '.'
