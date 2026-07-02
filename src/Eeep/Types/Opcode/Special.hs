{-# LANGUAGE UndecidableInstances #-}

{- |
Module: Eeep.Types.Opcode.Special

The @Special@ type.
-}

module Eeep.Types.Opcode.Special (
    -- * Types.
    Special (..),
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


{- | The t'Special' type. -}
newtype Special = Special Word32
    deriving stock (Eq, Ord, Bounded, Ix, Show)
    deriving newtype Enum


-- Instances.
instance (Source Word8 s, Parsers.Binary b s) => Reader s InputError Special where
    {-# INLINE parser #-}
    parser :: Parser s InputError Special
    parser = fmap Special Parsers.word32Le

instance (Sink Word8 b s, Serializers.Binary b s) => Writer b s Special where
    {-# INLINE serializer #-}
    serializer :: Serializer s Special
    serializer = contramap unwrap Serializers.word32Le
        where
            unwrap :: Special -> Word32
            unwrap (Special n) = n
