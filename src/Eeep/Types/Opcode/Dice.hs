{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module: Eeep.Types.Opcode.Dice

The @Dice@ type.
-}

module Eeep.Types.Opcode.Dice (
    -- * Types.
    Dice (..),
) where

-- Imports.
-- Base.
import Data.Word (Word32, Word8)
import GHC.Generics (Generic)

-- Libraries.
import Data.Functor.Contravariant.Divisible (Divisible(..))

-- non-Hackage libraries.
import Trisagion.Typeclasses.Source (Source)
import Trisagion.Typeclasses.Sink (Sink)
import Trisagion.Parser (Parser)
import Trisagion.Parsers.Source (InputError)
import qualified Trisagion.Parsers.Binary as Parsers
import Trisagion.Serializer (Serializer)
import qualified Trisagion.Serializers.Binary as Serializers

-- Package.
import Eeep.Typeclasses.Binary (Reader (..), Writer (..))


{- | The @Dice@ type. -}
data Dice = Dice {
    number :: !Word32,
    sides  :: !Word32
    } deriving stock (Eq, Ord, Generic, Show)


-- Instances.
instance (Source Word8 s, Parsers.Binary b s) => Reader s InputError Dice where
    {-# INLINE parser #-}
    parser :: Parser s InputError Dice
    parser = Dice <$> Parsers.word32Le <*> Parsers.word32Le

instance (Sink Word8 b s, Serializers.Binary b s) => Writer b s Dice where
    {-# INLINE serializer #-}
    serializer :: Serializer s Dice
    serializer = divide pair Serializers.word32Le Serializers.word32Le
        where
            pair :: Dice -> (Word32, Word32)
            pair (Dice n s) = (n, s)
