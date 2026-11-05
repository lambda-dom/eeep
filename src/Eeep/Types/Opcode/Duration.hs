{- |
Module: Eeep.Types.Opcode.Duration

The @Duration@ type.
-}

module Eeep.Types.Opcode.Duration (
    -- * Types.
    Duration,

    -- ** Isomorphisms.
    duration,

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


{- | The t'Duration' type. -}
newtype Duration = Duration Word32
    deriving stock (Eq, Ord, Bounded, Ix)
    deriving newtype (Enum, Show)


{- | Isomorphism for the t'Duration' type. -}
{-# INLINE duration #-}
duration :: Iso' Duration Word32
duration = coercedTo


{- | Decode a t'Duration'. -}
{-# INLINE decode #-}
decode :: Parsers.Binary b s => Parser s InputError Duration
decode = fmap (review duration) Parsers.word32Le

{- | Encode a t'Duration'. -}
{-# INLINE encode #-}
encode :: Serializers.Binary b s => Serializer s Duration
encode = contramap (view duration) Serializers.word32Le
