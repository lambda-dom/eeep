{-# LANGUAGE UndecidableInstances #-}

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
) where

-- Imports.
-- Base.
import Data.Functor.Contravariant (Contravariant (..))
import Data.Ix (Ix)
import Data.Word (Word8)

-- non-Hackage libraries.
import Trisagion.Utils.Either ((:+:))
import Trisagion.Typeclasses.Source (Source)
import Trisagion.Parser (Parser)
import Trisagion.Parsers.Combinators (validate)
import Trisagion.Parsers.Source (InputError, one)
import Trisagion.Serializer (Serializer)

-- Package.
import Eeep.Typeclasses.Binary (Reader (..), Writer (..))
import Eeep.Utils.Enum (eitherEnum)
import Trisagion.Serializers.Binary (Binary (word8))
import Trisagion.Typeclasses.Sink (Sink)


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


-- Instances.
instance Source Word8 s => Reader s (TargetError :+: InputError) Target where
    {-# INLINE parser #-}
    parser :: Parser s (TargetError :+: InputError) Target
    parser = validate target one

instance (Sink Word8 b s, Binary b s) => Writer b s Target where
    {-# INLINE serializer #-}
    serializer :: Serializer s Target
    serializer = contramap (fromIntegral . fromEnum) word8


{- | Smart constructor for the @t'Target'@ type. -}
{-# INLINE target #-}
target :: Word8 -> TargetError :+: Target
target n = eitherEnum (TargetError n) n
