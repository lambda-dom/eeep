{-# LANGUAGE UndecidableInstances #-}

{- |
Module: Eeep.Types.Opcode.Duration

The @Duration@ type.
-}

module Eeep.Types.Opcode.Duration (
    -- * Types.
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


{- | The @Duration@ type. -}
newtype Duration = Duration Word32
    deriving stock (Eq, Ord, Bounded, Ix, Show)
    deriving newtype Enum


-- Instances.
instance (Source Word8 s, Parsers.Binary b s) => Reader s InputError Duration where
    {-# INLINE parser #-}
    parser :: Parser s InputError Duration
    parser = fmap Duration Parsers.word32Le

instance (Sink Word8 b s, Serializers.Binary b s) => Writer b s Duration where
    {-# INLINE serializer #-}
    serializer :: Serializer s Duration
    serializer = contramap unwrap Serializers.word32Le
        where
            unwrap :: Duration -> Word32
            unwrap (Duration n) = n
