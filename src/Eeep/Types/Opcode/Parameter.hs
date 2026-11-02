{- |
Module: Eeep.Types.Opcode.Parameter

The @Parameter@ type.
-}

module Eeep.Types.Opcode.Parameter (
    -- * Types.
    Parameter,

    -- ** Isomorphisms.
    parameter,

    -- ** Parsers.
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


{- | The t'Parameter' type. -}
newtype Parameter = Parameter Word32
    deriving stock (Eq, Ord, Bounded, Ix, Show)
    deriving newtype Enum


{- | Isomorphism for the t'Parameter' type. -}
{-# INLINABLE parameter #-}
parameter :: Iso' Parameter Word32
parameter = coercedTo


{- | Decode a t'Parameter'. -}
{-# INLINE decode #-}
decode :: Binary b s => Parser s InputError Parameter
decode = fmap (review parameter) word32Le
