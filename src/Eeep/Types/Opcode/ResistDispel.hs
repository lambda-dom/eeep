{- |
Module: Eeep.Types.Opcode.ResistDispel

The @ResistDispel@ type.
-}

module Eeep.Types.Opcode.ResistDispel (
    -- * Error types.
    ResistDispelError (..),

    -- * Types.
    ResistDispel (..),

    -- ** Constructors.
    resistDispel,

    -- ** Parsers and serializers.
    encodeResistDispel,
    decodeResistDispel,
) where

-- Imports.
-- Base.
import Data.Functor.Contravariant (Contravariant (..))
import Data.Ix (Ix)
import Data.Word (Word8)

-- Libraries.
import Optics.Core (review)

-- non-Hackage libraries.
import Trisagion.Utils.Either ((:+:))
import Trisagion.Typeclasses.Source (Source)
import Trisagion.Parser (Parser)
import Trisagion.Parsers.Combinators (validate)
import Trisagion.Parsers.Source (InputError, one)
import Trisagion.Serializer (Serializer)
import Trisagion.Serializers.Binary (Binary, word8)

-- Package.
import Eeep.Utils.Enum (eitherEnum, enum)


{- | The t'ResistDispelError' type. -}
newtype ResistDispelError = ResistDispelError Word8
    deriving stock (Eq, Ord, Bounded, Ix, Show)
    deriving newtype Enum


{- | The @ResistDispel@ enumeration type. -}
data ResistDispel
    = Natural
    | DispellableResistable
    | UndispellableUnresistable
    | DispellableUnresistable
    deriving stock (Eq, Ord, Enum, Bounded, Ix, Show)


{- | Smart constructor for the @t'ResistDispel'@ values from 'Word8'. -}
{-# INLINE resistDispel #-}
resistDispel :: Word8 -> ResistDispelError :+: ResistDispel
resistDispel n = eitherEnum (ResistDispelError n) n


{- | Default parser for t'ResistDispel'. -}
{-# INLINE encodeResistDispel #-}
encodeResistDispel :: Source Word8 s => Parser s (ResistDispelError :+: InputError) ResistDispel
encodeResistDispel = validate resistDispel one

{- | Default serializer for t'ResistDispel'. -}
{-# INLINE decodeResistDispel #-}
decodeResistDispel :: Binary b s => Serializer s ResistDispel
decodeResistDispel = contramap (review enum) word8
