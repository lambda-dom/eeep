{- |
Module: Eeep.Types.Effect.School

The @School@ type.
-}

module Eeep.Types.Effect.School (
    -- * Types.
    School,

    -- * Parsers.
    decodeSchool,

    -- * Serializers.
    encodeSchool,
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


{- | The magic t'School' numeric id type. -}
newtype School = School Word32
    deriving stock (Eq, Show)


{- | Decode a t'School' numeric id. -}
decodeSchool
    :: (HasOffset s, Splittable s, MonoFoldable (PrefixOf s), ElementOf (PrefixOf s) ~ Word8)
    => Parser s InputError School
decodeSchool = capture . fmap School $ word32Le


{- | Encode a t'School' into a 'Word32'. -}
encodeSchool :: Binary m => Serializer m School
encodeSchool = contramap unwrap $ embed Binary.word32Le
    where
        unwrap :: School -> Word32
        unwrap (School n) = n
