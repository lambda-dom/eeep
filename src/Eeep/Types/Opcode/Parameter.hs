{-# LANGUAGE UndecidableInstances #-}

{- |
Module: Eeep.Types.Opcode.Parameter

The @Parameter@ type.
-}

module Eeep.Types.Opcode.Parameter (
    -- * Types.
    Parameter (..),
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


{- | The t'Parameter' type.

note(s):

    * Eventually, this type will be folded into a beefed up GADT OpcodeType.
-}
newtype Parameter = Parameter Word32
    deriving stock (Eq, Ord, Bounded, Ix, Show)
    deriving newtype Enum


-- Instances.
instance (Source Word8 s, Parsers.Binary b s) => Reader s InputError Parameter where
    {-# INLINE parser #-}
    parser :: Parser s InputError Parameter
    parser = fmap Parameter Parsers.word32Le

instance (Sink Word8 b s, Serializers.Binary b s) => Writer b s Parameter where
    {-# INLINE serializer #-}
    serializer :: Serializer s Parameter
    serializer = contramap unwrap Serializers.word32Le
        where
            unwrap :: Parameter -> Word32
            unwrap (Parameter n) = n
