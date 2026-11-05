{- |
Module: Eeep.Types.Opcode.Parameter

The @Parameter@ type.
-}

module Eeep.Types.Opcode.Parameter (
    -- * Types.
    Parameter,

    -- ** Isomorphisms.
    parameter,

    -- ** Parsers and serializers.
    decode,
    encode,
) where

-- Imports.
-- Base.
import Data.Functor.Contravariant (Contravariant (..))
import Data.Ix (Ix)
import Data.Word (Word32)

-- Libraries.
import Optics.Core (Iso', coercedTo, review, view)

-- non-Hackage libraries.
import qualified Trisagion.Typeclasses.Parsers.Binary as Parsers (Binary (..))
import Trisagion.Parser (Parser)
import Trisagion.Parsers.Source (InputError)
import qualified Trisagion.Typeclasses.Serializers.Binary as Serializers (Binary (..))
import Trisagion.Serializer (Serializer)


{- | The t'Parameter' type. -}
newtype Parameter = Parameter Word32
    deriving stock (Eq, Ord, Bounded, Ix)
    deriving newtype (Enum, Show)


{- | Isomorphism for the t'Parameter' type. -}
{-# INLINABLE parameter #-}
parameter :: Iso' Parameter Word32
parameter = coercedTo


{- | Decode a t'Parameter'. -}
{-# INLINE decode #-}
decode :: Parsers.Binary b s => Parser s InputError Parameter
decode = fmap (review parameter) Parsers.word32Le

{- | Encode a t'Parameter'. -}
{-# INLINE encode #-}
encode :: Serializers.Binary b s => Serializer s Parameter
encode = contramap (view parameter) Serializers.word32Le
