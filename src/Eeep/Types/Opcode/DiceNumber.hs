{- |
Module: Eeep.Types.Opcode.DiceNumber

The @DiceNumber@ type.
-}

module Eeep.Types.Opcode.DiceNumber (
    -- * Types.
    DiceNumber,

    -- * Parsers.
    decodeDiceNumber,

    -- * Serializers.
    encodeDiceNumber,
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
import Trisagion.Parser (Parser)
import Trisagion.Parsers.ParseError (capture)
import Trisagion.Parsers.Streamable (InputError)
import Trisagion.Parsers.Word8 (word32Le)
import Trisagion.Serializer (Serializer)
import qualified Trisagion.Serializers.Binary as Binary (word32Le)


{- | The t'DiceNumber' type. -}
newtype DiceNumber = DiceNumber Word32
    deriving stock (Eq, Ord, Show)


{- | Parse a t'DiceNumber'. -}
decodeDiceNumber
    :: (HasOffset s, Splittable s, MonoFoldable (PrefixOf s), ElementOf (PrefixOf s) ~ Word8)
    => Parser s InputError DiceNumber
decodeDiceNumber = capture . fmap DiceNumber $ word32Le


{- | Encode a t'DiceNumber' into a 'Word32'. -}
encodeDiceNumber :: Binary m => Serializer m DiceNumber
encodeDiceNumber = contramap unwrap Binary.word32Le
    where
        unwrap :: DiceNumber -> Word32
        unwrap (DiceNumber n) = n
