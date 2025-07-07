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

    -- * Types.
    parseResistDispel8,

    -- * Types.
    parseResistDispel32,
) where

-- Imports.
-- Base.
import Data.Bifunctor (Bifunctor (..))
import Data.Ix (Ix)
import Data.Void (absurd)
import Data.Word (Word8, Word32)

-- non-Hackage libraries.
import Mono.Typeclasses.MonoFunctor (ElementOf)
import Mono.Typeclasses.MonoFoldable (MonoFoldable)
import Trisagion.Types.ParseError (ParseError)
import Trisagion.Typeclasses.HasOffset (HasOffset)
import Trisagion.Typeclasses.Splittable (Splittable (PrefixOf))
import Trisagion.Parser (Parser)
import Trisagion.Parsers.ParseError (throwParseError, capture)
import Trisagion.Parsers.Word8 (word8, word32Le)


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
parseResistDispel8
    :: (HasOffset s, ElementOf s ~ Word8)
    => Parser s (ParseError ResistDispelError) ResistDispel
parseResistDispel8 = capture $ do
    n <- first (fmap absurd) word8
    maybe (throwParseError . ResistDispelError . fromIntegral $ n) pure (toResistDispel n)

{- | Parse a t'ResistDispel' from a single 'Word32'. -}
parseResistDispel32
    :: (HasOffset s, Splittable s, MonoFoldable (PrefixOf s), ElementOf (PrefixOf s) ~ Word8)
    => Parser s (ParseError ResistDispelError) ResistDispel
parseResistDispel32 = capture $ do
        n <- first (fmap absurd) word32Le
        if n > upper
            then throwParseError . ResistDispelError $ n
            else maybe (throwParseError . ResistDispelError $ n) pure (toResistDispel (fromIntegral n))
    where
        upper = fromIntegral (maxBound @Word8)
