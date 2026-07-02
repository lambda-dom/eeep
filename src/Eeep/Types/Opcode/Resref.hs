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
) where

-- Imports.
-- Base.
import Data.Char (isControl, isAscii)
import Data.Functor.Contravariant (Contravariant (..))
import Data.Ix (Ix)
import Data.Word (Word64, Word8)

-- Libraries.
import Optics.Core (review)

-- non-Hackage libraries.
import Trisagion.Utils.Either ((:+:))
import Trisagion.Utils.Bits (unpack)
import Trisagion.Typeclasses.Source (Source)
import Trisagion.Typeclasses.Sink (Sink)
import Trisagion.Parser (Parser)
import Trisagion.Parsers.Source (InputError)
import Trisagion.Serializer (Serializer)
import qualified Trisagion.Serializers.Binary as Serializers (Binary, word64Le)

-- Package.
import Eeep.Utils.Char (char)
import Eeep.Typeclasses.Binary (Writer (..), Reader (..))


{- | The t'ResrefError' type. -}
newtype ResrefError = ResrefError Char
    deriving stock (Eq, Ord, Bounded, Ix, Show)
    deriving newtype Enum


{- | The t'Resref' type for resource references. -}
newtype Resref = Resref Word64
    deriving stock (Eq, Ord)


-- Instances.
instance Show Resref where
    {-# INLINEABLE show #-}
    show :: Resref -> String
    show (Resref n) = "Resref '" ++ showBytes n ++ "'"
        where
            showBytes :: Word64 -> String
            showBytes = fmap (review char) . takeWhile (/= 0) . unpack

instance Source Word8 s => Reader s (ResrefError :+: InputError) Resref where
    {-# INLINE parser #-}
    parser :: Parser s (ResrefError :+: InputError) Resref
    parser = undefined

instance (Sink Word8 b s, Serializers.Binary b s) => Writer b s Resref where
    {-# INLINE serializer #-}
    serializer :: Serializer s Resref
    serializer = contramap unwrap Serializers.word64Le
        where
            -- Assumes normalized resref.
            unwrap :: Resref -> Word64
            unwrap (Resref n) = n


{- | Validate a 'Char' for a resource reference. -}
{-# INLINE validate #-}
validate :: Word8 -> ResrefError :+: Word8
validate n = if isValid c then Right n else Left $ ResrefError c
    where
        c :: Char
        c = review char n

        isValid :: Char -> Bool
        isValid d = isAscii d && not (isControl d) && d /= '\\' && d /= '/' && d /= '.'
