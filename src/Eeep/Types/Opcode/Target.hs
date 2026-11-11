{- |
Module: Eeep.Types.Opcode.Target

The @Target@ type.
-}

module Eeep.Types.Opcode.Target (
    -- * Error types.
    Error (..),

    -- * Types.
    Target (..),

    -- ** Prisms.
    target,

    -- ** Parsers and serializers.
    decode,
    encode,
) where

-- Imports.
-- Base.
import Data.Functor.Contravariant (Contravariant (..))
import Data.Ix (Ix)
import Data.Word (Word8)

-- Libraries.
import Optics.Core (Prism', review, preview, prism')

-- Non-hackage libraries.
import Data.Enum.Optics (enum)
import Data.Word.Optics (word8ToInt)
import Trisagion.Utils.Either ((:+:), validator)
import Trisagion.Parser (Parser)
import Trisagion.Parsers.Combinators (validate)
import Trisagion.Parsers.IStream (InputError)
import qualified Trisagion.Parsers.Binary as Parsers (Binary (..))
import Trisagion.Serializer (Serializer)
import qualified Trisagion.Serializers.Binary as Serializers (Binary (..))


{- | The t'Target' @Error@ type. -}
newtype Error = Error Word8
    deriving stock (Eq, Ord, Bounded, Show)
    deriving newtype Enum


{- | The @Target@ enumeration type for the target of an opcode. -}
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
{-# INLINE target #-}
target :: Prism' Word8 Target
target = prism' construct match
    where
        construct :: Target -> Word8
        construct = fromIntegral @Int @Word8 . review enum

        match :: Word8 -> Maybe Target
        match = preview enum . review word8ToInt


{- | Decode a t'Target'. -}
{-# INLINE decode #-}
decode :: Parsers.Binary b s => Parser s (InputError :+: Error) Target
decode = validate (validator Error (preview target)) Parsers.word8

{- | Encode a t'Target'. -}
{-# INLINE encode #-}
encode :: Serializers.Binary s => Serializer s Target
encode = contramap (review target) Serializers.word8
