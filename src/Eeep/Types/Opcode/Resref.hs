{- |
Module: Eeep.Types.Opcode.Resref

The @Resref@ type.
-}

module Eeep.Types.Opcode.Resref (
    -- * Types.
    Resref,

    -- * Parsers.
    decodeResref,

    -- * Serializers.
    encodeResref,
) where

-- Imports.
-- Base.
import Data.Bifunctor (Bifunctor (..))
import Data.Functor.Contravariant (Contravariant (..))
import Data.Char (isAscii, isControl, chr)
import Data.Void (absurd)
import Data.Word (Word8, Word64)

-- non-Hackage libraries.
import Mono.Typeclasses.MonoFunctor (ElementOf)
import Mono.Typeclasses.MonoFoldable (MonoFoldable (..))
import Mono.Types.ByteArray (bytes, pack)
import Trisagion.Types.ParseError (ParseError)
import Trisagion.Typeclasses.HasOffset (HasOffset)
import Trisagion.Typeclasses.Splittable (Splittable (PrefixOf))
import Trisagion.Typeclasses.Binary (Binary)
import Trisagion.Typeclasses.Builder (Builder (many))
import Trisagion.Parser (Parser)
import Trisagion.Parsers.ParseError (capture, throwParseError)
import Trisagion.Parsers.Splittable (takeExact)
import Trisagion.Serializer (Serializer, embed)


{- | The t'ResrefError' type. -}
newtype ResrefError = ResrefError Char
    deriving stock (Eq, Show)


{- | The t'Resref' type for resource references. -}
newtype Resref = Resref Word64
    deriving stock (Eq, Ord)

-- Instances.
instance Show Resref where
    show :: Resref -> String
    show (Resref n) = "Resref '" ++ showBytes n ++ "'"
        where
            showBytes :: Word64 -> String
            showBytes = fmap (chr . fromIntegral) . takeWhile (/= 0) . bytes


{- | Validate a 'Char' for a resource reference. -}
validate :: Word8 -> Either ResrefError Word8
validate n =
    let
        c = chr . fromIntegral $ n
    in
        if isValid c then Right n else Left . ResrefError $ c
    where
        isValid :: Char -> Bool
        isValid c = isAscii c && not (isControl c) && c /= '\\' && c /= '/' && c /= '.'

{- | Decode a resource t'Resref'. -}
decodeResref
    :: (HasOffset s, Splittable s, MonoFoldable (PrefixOf s), ElementOf (PrefixOf s) ~ Word8)
    => Parser s (ParseError ResrefError) Resref
decodeResref = capture $ do
    xs <- takeWhile (/= 0) . monotoList <$> first (fmap absurd) (takeExact 8)
    case mapM validate xs of
        Left  e  -> throwParseError e
        Right ys -> pure . Resref . pack $ ys


{- | Encode a resource t'Resref'. -}
encodeResref :: Binary m => Serializer m Resref
encodeResref = contramap unwrap $ embed many
    where
        unwrap :: Resref -> [Word8]
        unwrap (Resref n) = bytes n
