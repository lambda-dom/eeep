{- |
Module: Eeep.Types.Opcode.DiceSides

The @DiceSides@ type.
-}

module Eeep.Types.Opcode.DiceSides (
    -- * Types.
    DiceSides (..),

    -- * Parsers.
    parseDiceSides,
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

-- Package.


{- | The t'DiceSides' type. -}
newtype DiceSides = DiceSides Word32
    deriving stock (Eq, Ord, Show)


{- | Parse a t'DiceSides'. -}
parseDiceSides
    :: (HasOffset s, Splittable s, MonoFoldable (PrefixOf s), ElementOf (PrefixOf s) ~ Word8)
    => Parser s InputError DiceSides
parseDiceSides = capture . fmap DiceSides $ word32Le
