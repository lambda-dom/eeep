{- |
Module: Eeep.Types.Effect.School

The @School@ type.
-}

module Eeep.Types.Effect.School (
    -- * Types.
    School (..),

    -- * Parsers.
    parseSchool,
) where

-- Imports.
-- Base.
import Data.Word (Word8, Word32)

-- non-Hackage libraries.
import Mono.Typeclasses.MonoFunctor (ElementOf)
import Mono.Typeclasses.MonoFoldable (MonoFoldable)
import Trisagion.Typeclasses.HasOffset (HasOffset)
import Trisagion.Typeclasses.Splittable (Splittable (PrefixOf))
import Trisagion.Parser (Parser)
import Trisagion.Parsers.ParseError (capture)
import Trisagion.Parsers.Streamable (InputError)
import Trisagion.Parsers.Word8 (word32Le)


{- | The magic t'School' numeric id type. -}
newtype School = School Word32
    deriving stock (Eq, Show)


{- | Parse a t'School' numeric id. -}
parseSchool
    :: (HasOffset s, Splittable s, MonoFoldable (PrefixOf s), ElementOf (PrefixOf s) ~ Word8)
    => Parser s InputError School
parseSchool = capture . fmap School $ word32Le
