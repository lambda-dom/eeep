{- |
Module: Eeep.Types.Opcode.Special

The @Special@ type.
-}

module Eeep.Types.Opcode.Special (
    -- * Types.
    Special (..),

    -- * Parsers.
    decodeSpecial,

    -- * Serializers.
    encodeSpecial,
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


{- | The t'Special' type. -}
newtype Special = Special Word32
    deriving stock (Eq, Ord, Show)


{- | Decode the opcode's t'Special' field. -}
decodeSpecial
    :: (HasOffset s, Splittable s, MonoFoldable (PrefixOf s), ElementOf (PrefixOf s) ~ Word8)
    => Parser s InputError Special
decodeSpecial = capture . fmap Special $ word32Le


{- | Encode a t'Special' into a 'Word32'. -}
encodeSpecial :: Binary m => Serializer m Special
encodeSpecial = contramap unwrap $ embed Binary.word32Le
    where
        unwrap :: Special -> Word32
        unwrap (Special n) = n
