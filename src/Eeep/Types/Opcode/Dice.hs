{-# LANGUAGE NoFieldSelectors #-}

{- |
Module: Eeep.Types.Opcode.Dice

The @Dice@ type.
-}

module Eeep.Types.Opcode.Dice (
    -- * Types.
    Dice (..),

    -- ** Parsers and serializers.
    encodeDice,
    decodeDice,
) where

-- Imports.
-- Base.
import Data.Word (Word32)
import GHC.Generics (Generic)

-- Libraries.
import Data.Functor.Contravariant.Divisible (Divisible(..))

-- non-Hackage libraries.
import Trisagion.Parser (Parser)
import Trisagion.Parsers.Source (InputError)
import qualified Trisagion.Parsers.Binary as Parsers
import Trisagion.Serializer (Serializer)
import qualified Trisagion.Serializers.Binary as Serializers


{- | The @Dice@ type. -}
data Dice = Dice {
    number :: {-# UNPACK #-} !Word32,
    sides  :: {-# UNPACK #-} !Word32
    } deriving stock (Eq, Ord, Generic, Show)


{- | Default parser for t'Dice'. -}
{-# INLINE encodeDice #-}
encodeDice :: Parsers.Binary b s => Parser s InputError Dice
encodeDice = Dice <$> Parsers.word32Le <*> Parsers.word32Le

{- | Default serializer for t'Dice'. -}
{-# INLINE decodeDice #-}
decodeDice :: Serializers.Binary b s => Serializer s Dice
decodeDice = divide pair Serializers.word32Le Serializers.word32Le
    where
        pair :: Dice -> (Word32, Word32)
        pair (Dice n s) = (n, s)
