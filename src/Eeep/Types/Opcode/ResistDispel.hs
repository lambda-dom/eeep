{- |
Module: Eeep.Types.Opcode.ResistDispel

The @ResistDispel@ type.
-}

module Eeep.Types.Opcode.ResistDispel (
    -- * Error types.
    ResistDispelError (..),

    -- * Types.
    ResistDispel,

    -- ** Constructors.
    toResistDispel,

    -- * Parsers.
    decodeResistDispel8,
    decodeResistDispel32,

    -- * Serializers.
    encodeResistDispel8,
    encodeResistDispel32,
) where

-- Imports.
-- Base.
import Data.Bifunctor (Bifunctor (..))
import Data.Functor.Contravariant (Contravariant (..))
import Data.Ix (Ix)
import Data.Void (absurd)
import Data.Word (Word8, Word32)

-- non-Hackage libraries.
import Mono.Typeclasses.MonoFunctor (ElementOf)
import Mono.Typeclasses.MonoFoldable (MonoFoldable)
import Trisagion.Types.ParseError (ParseError)
import Trisagion.Typeclasses.HasOffset (HasOffset)
import Trisagion.Typeclasses.Splittable (Splittable (PrefixOf))
import Trisagion.Typeclasses.Binary (Binary)
import qualified Trisagion.Typeclasses.Builder as Builder (one)
import qualified Trisagion.Typeclasses.Binary as Binary (word32Le)
import Trisagion.Parser (Parser)
import Trisagion.Parsers.ParseError (throwParseError, capture)
import Trisagion.Parsers.Word8 (word8, word32Le)
import Trisagion.Serializer (Serializer, embed)


{- | The t'ResistDispelError' type. -}
newtype ResistDispelError = ResistDispelError Word32
    deriving stock (Eq, Show)


{- | The @ResistDispel@ enumeration type. -}
data ResistDispel
    = Natural
    | DispellableResistable
    | UndispellableUnresistable
    | DispellableUnresistable
    deriving stock (Eq, Ord, Enum, Bounded, Ix, Show)


{- | Smart constructor for the 'ResistDispel' type.-}
{-# INLINE toResistDispel #-}
toResistDispel :: Word8 -> Maybe ResistDispel
toResistDispel n = if n <= 3 then Just $ toEnum (fromIntegral n) else Nothing


{- | Parse a t'ResistDispel' from a 'Word8'. -}
decodeResistDispel8
    :: (HasOffset s, ElementOf s ~ Word8)
    => Parser s (ParseError ResistDispelError) ResistDispel
decodeResistDispel8 = capture $ do
    n <- first (fmap absurd) word8
    maybe (throwParseError . ResistDispelError . fromIntegral $ n) pure (toResistDispel n)

{- | Parse a t'ResistDispel' from a single 'Word32'. -}
decodeResistDispel32
    :: (HasOffset s, Splittable s, MonoFoldable (PrefixOf s), ElementOf (PrefixOf s) ~ Word8)
    => Parser s (ParseError ResistDispelError) ResistDispel
decodeResistDispel32 = capture $ do
        n <- first (fmap absurd) word32Le
        if n > upper
            then throwParseError . ResistDispelError $ n
            else maybe (throwParseError . ResistDispelError $ n) pure (toResistDispel (fromIntegral n))
    where
        upper = fromIntegral (maxBound @Word8)

{- | Encode a t'ResistDispel' into a 'Word8'. -}
encodeResistDispel8 :: Binary m => Serializer m ResistDispel
encodeResistDispel8 = contramap (fromIntegral . fromEnum) $ embed Builder.one

{- | Encode a t'ResistDispel' into a 'Word32'. -}
encodeResistDispel32 :: Binary m => Serializer m ResistDispel
encodeResistDispel32 = contramap (fromIntegral . fromEnum) $ embed Binary.word32Le
