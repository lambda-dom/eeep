{- |
Module: Eeep.Types.Opcode.DiceNumber

The @DiceNumber@ type.
-}

module Eeep.Types.Opcode.DiceNumber (
    -- * Types.
    DiceNumber (..),

    -- * Parsers.
    parseDiceNumber,
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


{- | The t'DiceNumber' type. -}
newtype DiceNumber = DiceNumber Word32
    deriving stock (Eq, Ord, Show)


{- | Parse a t'DiceNumber'. -}
parseDiceNumber
    :: (HasOffset s, Splittable s, MonoFoldable (PrefixOf s), ElementOf (PrefixOf s) ~ Word8)
    => Parser s InputError DiceNumber
parseDiceNumber = capture . fmap DiceNumber $ word32Le
