{- |
Module: Eeep.Types.Opcode.Resref

The @Resref@ type.
-}

module Eeep.Types.Opcode.Resref (
    -- * Types.
    Resref (..),

    -- * Parsers.
    parseResref,
) where

-- Imports.
-- Base.
import Data.Word (Word8, Word64)

-- non-Hackage libraries.
import Mono.Typeclasses.MonoFunctor (ElementOf)
import Mono.Typeclasses.MonoFoldable (MonoFoldable)
import Trisagion.Typeclasses.HasOffset (HasOffset)
import Trisagion.Typeclasses.Splittable (Splittable (PrefixOf))
import Trisagion.Parser (Parser)
import Trisagion.Parsers.Streamable (InputError)
import Trisagion.Parsers.ParseError (capture)
import Trisagion.Parsers.Word8 (word64Le)


{- | The t'Resref' type for resource references. -}
newtype Resref = Resref Word64
    deriving stock (Eq, Ord, Show)


{- | Parse a resource t'Resref'. -}
parseResref
    :: (HasOffset s, Splittable s, MonoFoldable (PrefixOf s), ElementOf (PrefixOf s) ~ Word8)
    => Parser s InputError Resref
parseResref = capture . fmap Resref $ word64Le
