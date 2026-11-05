{- |
Module: Eeep.Types.Opcode.Special

The @Special@ type.
-}

module Eeep.Types.Opcode.Special (
    -- * Types.
    Special,

    -- ** Isomorphisms.
    special,

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


{- | The t'Special' type. -}
newtype Special = Special Word32
    deriving stock (Eq, Ord, Bounded, Ix)
    deriving newtype (Enum, Show)


{- | Isomorphism for the t'Special' type. -}
{-# INLINABLE special #-}
special :: Iso' Special Word32
special = coercedTo


{- | Decode a t'Special'. -}
{-# INLINE decode #-}
decode :: Parsers.Binary b s => Parser s InputError Special
decode = fmap (review special) Parsers.word32Le

{- | Encode a t'Special'. -}
{-# INLINE encode #-}
encode :: Serializers.Binary b s => Serializer s Special
encode = contramap (view special) Serializers.word32Le
