{-# LANGUAGE UndecidableInstances #-}

{- |
Module: Eeep.Types.Opcode.Resref

The @Resref@ type.
-}

module Eeep.Types.Opcode.Resref (
    -- * Types.
    Resref (..),
) where

-- Imports.
-- Base.
import Data.Void (Void)
import Data.Word (Word8, Word64)

-- non-Hackage libraries.
import Mono.Typeclasses.MonoFunctor (ElementOf)
import Mono.Typeclasses.MonoFoldable (MonoFoldable)
import Trisagion.Types.ParseError (ParseError)
import Trisagion.Typeclasses.HasOffset (HasOffset)
import Trisagion.Typeclasses.Splittable (Splittable (PrefixOf))
import Trisagion.Parser (Parser)
import Trisagion.Parsers.ParseError (capture)
import Trisagion.Parsers.Word8 (word64Le)

-- Package.
import Eeep.Typeclasses.Binary (Reader (..))


{- | The t'Resref' type for resource references. -}
newtype Resref = Resref Word64
    deriving stock (Eq, Ord, Show)


-- Instances.
instance (HasOffset s, Splittable s, MonoFoldable (PrefixOf s), ElementOf (PrefixOf s) ~ Word8)
    => Reader s Void Resref where
    parser :: Parser s (ParseError Void) Resref
    parser = capture . fmap Resref $ word64Le
