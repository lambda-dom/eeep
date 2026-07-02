{-# LANGUAGE UndecidableInstances #-}

{- |
Module: Eeep.Types.Opcode.DiceSides

The @DiceSides@ type.
-}

module Eeep.Types.Opcode.DiceSides (
    -- * Types.
    DiceSides (..),
) where

-- Imports.
-- Base.
import Data.Functor.Contravariant (Contravariant (..))
import Data.Ix (Ix)
import Data.Word (Word32, Word8)

-- non-Hackage libraries.
import Trisagion.Typeclasses.Source (Source)
import Trisagion.Typeclasses.Sink (Sink)
import Trisagion.Parser (Parser)
import Trisagion.Parsers.Source (InputError)
import qualified Trisagion.Parsers.Binary as Parsers (Binary, word32Le)
import Trisagion.Serializer (Serializer)
import qualified Trisagion.Serializers.Binary as Serializers (Binary, word32Le)

-- Package.
import Eeep.Typeclasses.Binary (Reader (..), Writer (..))


{- | The t'DiceSides' type. -}
newtype DiceSides = DiceSides Word32
    deriving stock (Eq, Ord, Bounded, Ix, Show)
    deriving newtype Enum


-- Instances.
instance (Source Word8 s, Parsers.Binary b s) => Reader s InputError DiceSides where
    {-# INLINE parser #-}
    parser :: Parser s InputError DiceSides
    parser = fmap DiceSides Parsers.word32Le

instance (Sink Word8 b s, Serializers.Binary b s) => Writer b s DiceSides where
    {-# INLINE serializer #-}
    serializer :: Serializer s DiceSides
    serializer = contramap unwrap Serializers.word32Le
        where
            unwrap :: DiceSides -> Word32
            unwrap (DiceSides n) = n
