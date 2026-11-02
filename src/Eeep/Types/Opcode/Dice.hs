{-# LANGUAGE NoFieldSelectors #-}

{- |
Module: Eeep.Types.Opcode.Dice

The @Dice@ type.
-}

module Eeep.Types.Opcode.Dice (
    -- * Types.
    Dice,

    -- ** Isomorphisms.
    dice,

    -- ** Parsers.
    decode,
) where

-- Imports.
-- Base.
import Data.Word (Word32)
import GHC.Generics (Generic)

-- Libraries.
import Optics.Core (Iso', iso, review)

-- non-Hackage libraries.
import Trisagion.Typeclasses.Parsers.Binary (Binary (..))
import Trisagion.Parser (Parser)
import Trisagion.Parsers.Combinators (zipP)
import Trisagion.Parsers.Source (InputError)


{- | The @Dice@ type. -}
data Dice = Dice {
    number :: {-# UNPACK #-} !Word32,
    sides  :: {-# UNPACK #-} !Word32
} deriving stock (Eq, Ord, Generic, Show)


{- | Isomorphism for the t'Dice' type. -}
{-# INLINABLE dice #-}
dice :: Iso' Dice (Word32, Word32)
dice = iso from to
    where
        from :: Dice -> (Word32, Word32)
        from (Dice m n) = (m, n)

        to :: (Word32, Word32) -> Dice
        to (m, n) = Dice {number = m, sides = n}


{- | Decode a t'Dice'. -}
{-# INLINE decode #-}
decode :: Binary b s => Parser s InputError Dice
decode = fmap (review dice) $ zipP word32Le word32Le
