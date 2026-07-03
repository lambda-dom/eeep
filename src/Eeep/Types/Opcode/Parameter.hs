{- |
Module: Eeep.Types.Opcode.Parameter

The @Parameter@ type.
-}

module Eeep.Types.Opcode.Parameter (
    -- * Types.
    Parameter (..),

    -- ** Parsers and serializers.
    encodeParameter,
    decodeParameter,
) where

-- Imports.
-- Base.
import Data.Functor.Contravariant (Contravariant (..))
import Data.Ix (Ix)
import Data.Word (Word32)

-- non-Hackage libraries.
import Trisagion.Parser (Parser)
import Trisagion.Parsers.Source (InputError)
import qualified Trisagion.Parsers.Binary as Parsers (Binary, word32Le)
import Trisagion.Serializer (Serializer)
import qualified Trisagion.Serializers.Binary as Serializers (Binary, word32Le)


{- | The t'Parameter' type.

note(s):

    * Eventually, this type will be folded into a beefed up GADT OpcodeType.
-}
newtype Parameter = Parameter Word32
    deriving stock (Eq, Ord, Bounded, Ix, Show)
    deriving newtype Enum


{- | Default parser for t'Parameter'. -}
{-# INLINE encodeParameter #-}
encodeParameter :: Parsers.Binary b s => Parser s InputError Parameter
encodeParameter = fmap Parameter Parsers.word32Le

{- | Default serializer for t'Parameter'. -}
{-# INLINE decodeParameter #-}
decodeParameter :: Serializers.Binary b s => Serializer s Parameter
decodeParameter = contramap unwrap Serializers.word32Le
    where
        unwrap :: Parameter -> Word32
        unwrap (Parameter n) = n
