{- |
Module: Eeep.Types.Opcode.Parameter

The @Parameter@ type.
-}

module Eeep.Types.Opcode.Parameter (
    -- * Types.
    Parameter,

    -- * Parsers.
    decodeParameter,

    -- * Serializers.
    encodeParameter,
) where

-- Imports.
-- Base.
import Data.Functor.Contravariant (Contravariant (..))
import Data.Word (Word8, Word32)

-- non-Hackage libraries.
import Mono.Typeclasses.MonoFunctor (ElementOf)
import Mono.Typeclasses.MonoFoldable (MonoFoldable)
import Trisagion.Typeclasses.HasOffset (HasOffset)
import Trisagion.Typeclasses.Splittable (Splittable (PrefixOf))
import Trisagion.Typeclasses.Binary (Binary)
import qualified Trisagion.Typeclasses.Binary as Binary (word32Le)
import Trisagion.Parser (Parser)
import Trisagion.Parsers.ParseError (capture)
import Trisagion.Parsers.Streamable (InputError)
import Trisagion.Parsers.Word8 (word32Le)
import Trisagion.Serializer (Serializer, embed)


{- | The t'Parameter' type. -}
newtype Parameter = Parameter Word32
    deriving stock (Eq, Ord, Show)


{- | Decode a t'Parameter'. -}
decodeParameter
    :: (HasOffset s, Splittable s, MonoFoldable (PrefixOf s), ElementOf (PrefixOf s) ~ Word8)
    => Parser s InputError Parameter
decodeParameter = capture . fmap Parameter $ word32Le


{- | Encode a t'Parameter' into a 'Word32'. -}
encodeParameter :: Binary m => Serializer m Parameter
encodeParameter = contramap unwrap $ embed Binary.word32Le
    where
        unwrap :: Parameter -> Word32
        unwrap (Parameter n) = n
