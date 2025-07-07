{- |
Module: Eeep.Types.Effect.Sectype

The @Sectype@ type.
-}

module Eeep.Types.Effect.Sectype (
    -- * Types.
    Sectype (..),

    -- * Parsers.
    parseSectype,
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


{- | The magic t'Sectype' numeric id type. -}
newtype Sectype = Sectype Word32
    deriving stock (Eq, Show)


{- | Parse a t'Sectype' numeric id. -}
parseSectype
    :: (HasOffset s, Splittable s, MonoFoldable (PrefixOf s), ElementOf (PrefixOf s) ~ Word8)
    => Parser s InputError Sectype
parseSectype = capture . fmap Sectype $ word32Le
