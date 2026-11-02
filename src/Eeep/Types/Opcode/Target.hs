{- |
Module: Eeep.Types.Opcode.Target

The @Target@ type.
-}

module Eeep.Types.Opcode.Target (
    -- * Error types.
    TargetError (..),

    -- * Types.
    Target (..),

    -- ** Prisms.
    target,

    -- ** Parsers.
    decode,
) where

-- Imports.
-- Base.
import Data.Ix (Ix)
import Data.Word (Word8)

-- Libraries.
import Optics.Core (Prism', review, preview, prism')

-- Non-hackage libraries.
import Data.Enum.Optics (enum)
import Trisagion.Utils.Either ((:+:), annotate)
import Trisagion.Typeclasses.Parsers.Binary (Binary (..))
import Trisagion.Parser (Parser)
import Trisagion.Parsers.Combinators (validate)
import Trisagion.Parsers.Source (InputError)


{- | The @TargetError@ type. -}
newtype TargetError = TargetError Word8
    deriving stock (Eq, Ord, Bounded, Show)


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


{- | Prism for the @t'Target'@ type. -}
{-# INLINABLE target #-}
target :: Prism' Word8 Target
target = prism' construct match
    where
        construct :: Target -> Word8
        construct = fromIntegral @Int @Word8 . review enum

        match :: Word8 -> Maybe Target
        match = preview enum . fromIntegral @Word8 @Int


{- | Decode a t'Target'. -}
{-# INLINE decode #-}
decode :: Binary b s => Parser s (TargetError :+: InputError) Target
decode = validate (annotate TargetError (preview target)) word8
