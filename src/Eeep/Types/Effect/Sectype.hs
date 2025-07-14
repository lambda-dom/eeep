{- |
Module: Eeep.Types.Effect.Sectype

The @Sectype@ type.
-}

module Eeep.Types.Effect.Sectype (
    -- * Types.
    Sectype (..),

    -- * Parsers.
    decodeSectype,

    -- * Serializers.
    encodeSectype,
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


{- | The magic t'Sectype' numeric id type. -}
newtype Sectype = Sectype Word32
    deriving stock (Eq, Show)


{- | Parse a t'Sectype' numeric id. -}
decodeSectype
    :: (HasOffset s, Splittable s, MonoFoldable (PrefixOf s), ElementOf (PrefixOf s) ~ Word8)
    => Parser s InputError Sectype
decodeSectype = capture . fmap Sectype $ word32Le


{- | Encode a t'Sectype' into a 'Word32'. -}
encodeSectype :: Binary m => Serializer m Sectype
encodeSectype = contramap unwrap $ embed Binary.word32Le
    where
        unwrap :: Sectype -> Word32
        unwrap (Sectype n) = n
