{-# LANGUAGE UndecidableInstances #-}

{- |
Module: Eeep.Types.Opcode.Parameter

The @Parameter@ type.
-}

module Eeep.Types.Opcode.Parameter (
    -- * Types.
    Parameter (..)
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


{- | The t'Parameter' type. -}
newtype Parameter = Parameter Word32
    deriving stock (Eq, Ord, Show)

-- Instances.
instance (HasOffset s, Splittable s, MonoFoldable (PrefixOf s), ElementOf (PrefixOf s) ~ Word8)
    => Reader s Void Parameter where
    parser :: Parser s InputError Parameter
    parser = capture . fmap Parameter $ word32Le
