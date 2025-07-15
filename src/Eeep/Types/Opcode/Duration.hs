{- |
Module: Eeep.Types.Opcode.Duration

The @Duration@ type.
-}

module Eeep.Types.Opcode.Duration (
    -- * Types.
    Duration,

    -- * Parsers.
    decodeDuration,

    -- * Serializers.
    encodeDuration,
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


{- | The t'Duration' type. -}
newtype Duration = Duration Word32
    deriving stock (Eq, Ord, Show)


{- | Parse a timing t'Duration'. -}
decodeDuration
    :: (HasOffset s, Splittable s, MonoFoldable (PrefixOf s), ElementOf (PrefixOf s) ~ Word8)
    => Parser s InputError Duration
decodeDuration = capture . fmap Duration $ word32Le

{- | Encode a t'Duration' into a 'Word32'. -}
encodeDuration :: Binary m => Serializer m Duration
encodeDuration = contramap unwrap Binary.word32Le
    where
        unwrap :: Duration -> Word32
        unwrap (Duration n) = n
