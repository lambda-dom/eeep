{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module: Eeep.Types.Opcode.Dice

The @Dice@ type.
-}

module Eeep.Types.Opcode.Dice (
    -- * Types.
    Dice,

    -- ** Isomorphisms.
    dice,

    -- ** Parsers and serializers.
    decode,
    encode,
) where

-- Imports.
-- Base.
import GHC.Generics (Generic)
import Data.Functor.Contravariant (Contravariant (..))
import Data.Word (Word32)

-- Libraries.
import Data.Functor.Contravariant.Divisible (divided)
import Optics.Core (Iso', iso, review, view)

-- non-Hackage libraries.
import qualified Trisagion.Typeclasses.Parsers.Binary as Parsers (Binary (..))
import Trisagion.Parser (Parser)
import Trisagion.Parsers.Combinators (zipP)
import Trisagion.Parsers.Source (InputError)
import qualified Trisagion.Typeclasses.Serializers.Binary as Serializers (Binary (..))
import Trisagion.Serializer (Serializer)


{- | The @Dice@ type. -}
data Dice = Dice {
    number :: {-# UNPACK #-} !Word32,
    sides  :: {-# UNPACK #-} !Word32
} deriving stock (Eq, Ord, Bounded, Show, Generic)


{- | Isomorphism for the t'Dice' type. -}
{-# INLINE dice #-}
dice :: Iso' Dice (Word32, Word32)
dice = iso from to
    where
        from :: Dice -> (Word32, Word32)
        from (Dice m n) = (m, n)

        to :: (Word32, Word32) -> Dice
        to (number, sides) = Dice {..}


{- | Decode a t'Dice'. -}
{-# INLINE decode #-}
decode :: Parsers.Binary b s => Parser s InputError Dice
decode = fmap (review dice) $ zipP Parsers.word32Le Parsers.word32Le

{- | Encode a t'Dice'. -}
{-# INLINE encode #-}
encode :: Serializers.Binary b s => Serializer s Dice
encode = contramap (view dice) (divided Serializers.word32Le Serializers.word32Le)
