{- |
Module: Eeep.Types.Opcode.Special

The @Special@ type.
-}

module Eeep.Types.Opcode.Special (
    -- * Types.
    Special (..),

    -- ** Parsers and serializers.
    encodeSpecial,
    decodeSpecial,
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


{- | The t'Special' type. -}
newtype Special = Special Word32
    deriving stock (Eq, Ord, Bounded, Ix, Show)
    deriving newtype Enum


{- | Default parser for t'Special'. -}
{-# INLINE encodeSpecial #-}
encodeSpecial :: Parsers.Binary b s => Parser s InputError Special
encodeSpecial = fmap Special Parsers.word32Le

{- | Default serializer for t'Special'. -}
{-# INLINE decodeSpecial #-}
decodeSpecial :: Serializers.Binary b s => Serializer s Special
decodeSpecial = contramap unwrap Serializers.word32Le
    where
        unwrap :: Special -> Word32
        unwrap (Special n) = n
