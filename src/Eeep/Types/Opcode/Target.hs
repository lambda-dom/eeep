{- |
Module: Eeep.Types.Opcode.Target

The @Target@ type.
-}

module Eeep.Types.Opcode.Target (
    -- * Error types.
    TargetError (..),

    -- * Types.
    Target (..),

    -- ** Constructors.
    target,

    -- ** Parsers and serializers.
    encodeTarget,
    decodeTarget,
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
import Trisagion.Serializers.Binary (Binary (word8))

-- Package.
import Eeep.Utils.Enum (eitherEnum, enum)


{- | The t'TargetError' error type. -}
newtype TargetError = TargetError Word8
    deriving stock (Eq, Ord, Bounded, Ix, Show)
    deriving newtype Enum


{- | The @Target@ enumeration type. -}
data Target
    = None
    | Self
    | Preset
    | Party
    | Area
    | NotParty
    | CasterGroup
    | TargetGroup
    | NotSelf
    | Original
    deriving stock (Eq, Ord, Enum, Bounded, Ix, Show)


{- | Smart constructor for the @t'Target'@ type. -}
{-# INLINE target #-}
target :: Word8 -> TargetError :+: Target
target n = eitherEnum (TargetError n) n


{- | Default parser for t'Target'. -}
{-# INLINE encodeTarget #-}
encodeTarget :: Source Word8 s => Parser s (TargetError :+: InputError) Target
encodeTarget = validate target one

{- | Default serializer for t'Target'. -}
{-# INLINE decodeTarget #-}
decodeTarget :: Binary b s => Serializer s Target
decodeTarget = contramap (review enum) word8
