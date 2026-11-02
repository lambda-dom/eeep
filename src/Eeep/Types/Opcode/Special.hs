{- |
Module: Eeep.Types.Opcode.Special

The @Special@ type.
-}

module Eeep.Types.Opcode.Special (
    -- * Types.
    Special,

    -- ** Isomorphisms.
    special,

    -- * Parsers.
    decode,
) where

-- Imports.
-- Base.
import Data.Ix (Ix)
import Data.Word (Word32)

-- Libraries.
import Optics.Core (Iso', coercedTo, review)

-- non-Hackage libraries.
import Trisagion.Typeclasses.Parsers.Binary (Binary (..))
import Trisagion.Parser (Parser)
import Trisagion.Parsers.Source (InputError)


{- | The t'Special' type. -}
newtype Special = Special Word32
    deriving stock (Eq, Ord, Bounded, Ix, Show)
    deriving newtype Enum


{- | Isomorphism for the t'Special' type. -}
{-# INLINABLE special #-}
special :: Iso' Special Word32
special = coercedTo


{- | Decode a t'Special'. -}
{-# INLINE decode #-}
decode :: Binary b s => Parser s InputError Special
decode = fmap (review special) word32Le
