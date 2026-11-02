{- |
Module: Eeep.Types.Opcode.Duration

The @Duration@ type.
-}

module Eeep.Types.Opcode.Duration (
    -- * Types.
    Duration,

    -- ** Isomorphisms.
    duration,

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


{- | The t'Duration' type. -}
newtype Duration = Duration Word32
    deriving stock (Eq, Ord, Bounded, Ix, Show)
    deriving newtype Enum


{- | Isomorphism for the t'Duration' type. -}
{-# INLINE duration #-}
duration :: Iso' Duration Word32
duration = coercedTo


{- | Decode a t'Duration'. -}
{-# INLINE decode #-}
decode :: Binary b s => Parser s InputError Duration
decode = fmap (review duration) word32Le
