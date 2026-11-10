{- |
Module: Eeep.Types.Shared.Ability.Range

The ability @Range@ type.
-}

module Eeep.Types.Shared.Ability.Range (
    -- * Types.
    Range,

    -- ** Prisms.
    range,

    -- ** Parsers and serializers.
    decode,
    encode,
) where

-- Imports.
-- Base.
import Data.Functor.Contravariant (Contravariant (..))
import Data.Ix (Ix)
import Data.Word (Word16)

-- Libraries.
import Optics.Core (Iso', review, coercedTo, view)

-- non-Hackage libraries.
import qualified Trisagion.Typeclasses.Parsers.Binary as Parsers (Binary (..))
import Trisagion.Parser (Parser)
import Trisagion.Parsers.Source (InputError)
import qualified Trisagion.Typeclasses.Serializers.Binary as Serializers (Binary (..))
import Trisagion.Serializer (Serializer)


{- | The @Range@ type. -}
newtype Range = Range Word16
    deriving stock (Eq, Ord, Bounded, Ix)
    deriving newtype (Enum, Show)


{- | Isomorphism for the t'Range' type. -}
{-# INLINE range #-}
range :: Iso' Range Word16
range = coercedTo


{- | Decode a t'Range'. -}
{-# INLINE decode #-}
decode :: Parsers.Binary b s => Parser s InputError Range
decode = fmap (review range) Parsers.word16Le

{- | Encode a t'Range'. -}
{-# INLINE encode #-}
encode :: Serializers.Binary b s => Serializer s Range
encode = contramap (view range) Serializers.word16Le
