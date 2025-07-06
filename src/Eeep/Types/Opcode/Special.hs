{-# LANGUAGE UndecidableInstances #-}

{- |
Module: Eeep.Types.Opcode.Special

The @Special@ type.
-}

module Eeep.Types.Opcode.Special (
    -- * Types.
    Special (..)
) where

-- Imports.
-- Base.
import Data.Void (Void)
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
import Eeep.Typeclasses.Binary (Reader (..))


{- | The t'Special' type. -}
newtype Special = Special Word32
    deriving stock (Eq, Ord, Show)


-- Instances.
instance (HasOffset s, Splittable s, MonoFoldable (PrefixOf s), ElementOf (PrefixOf s) ~ Word8)
    => Reader s Void Special where
    parser :: Parser s InputError Special
    parser = capture . fmap Special $ word32Le
