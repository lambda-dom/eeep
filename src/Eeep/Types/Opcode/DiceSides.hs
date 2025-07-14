{- |
Module: Eeep.Types.Opcode.DiceSides

The @DiceSides@ type.
-}

module Eeep.Types.Opcode.DiceSides (
    -- * Types.
    DiceSides,

    -- * Parsers.
    decodeDiceSides,

    -- * Serializers.
    encodeDiceSides,
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


{- | The t'DiceSides' type. -}
newtype DiceSides = DiceSides Word32
    deriving stock (Eq, Ord, Show)


{- | Decode a t'DiceSides'. -}
decodeDiceSides
    :: (HasOffset s, Splittable s, MonoFoldable (PrefixOf s), ElementOf (PrefixOf s) ~ Word8)
    => Parser s InputError DiceSides
decodeDiceSides = capture . fmap DiceSides $ word32Le


{- | Encode a t'DiceSides' into a 'Word32'. -}
encodeDiceSides :: Binary m => Serializer m DiceSides
encodeDiceSides = contramap unwrap $ embed Binary.word32Le
    where
        unwrap :: DiceSides -> Word32
        unwrap (DiceSides n) = n
