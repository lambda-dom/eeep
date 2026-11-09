{- |
Module: Eeep.Types.Shared.Proficiency

The @Proficiency@ type.
-}

module Eeep.Types.Shared.Proficiency (
    -- * Types.
    Proficiency (..),

    -- ** Isomorphisms.
    proficiency,

    -- ** Parsers and serializers.
    decode,
    encode,
) where

-- Imports.
-- Base.
import Data.Functor.Contravariant (Contravariant (..))
import Data.Ix (Ix)
import Data.Word (Word8)

-- Libraries.
import Optics.Core (Iso', coercedTo, view, review)

-- non-Hackage libraries.
import qualified Trisagion.Typeclasses.Parsers.Binary as Parsers (Binary (..))
import Trisagion.Parser (Parser)
import Trisagion.Parsers.Source (InputError)
import qualified Trisagion.Typeclasses.Serializers.Binary as Serializers (Binary (..))
import Trisagion.Serializer (Serializer)


{- | The t'Proficiency' numeric id type for weapon proficiency.

note(s):

    * t'Proficiency' values can only be validated in the 'IO' monad.
-}
newtype Proficiency = Proficiency Word8
    deriving stock (Eq, Ord, Bounded, Ix)
    deriving newtype (Enum, Show)


{- | Isomorphism for the t'Proficiency' type. -}
{-# INLINE proficiency #-}
proficiency :: Iso' Proficiency Word8
proficiency = coercedTo


{- | Decode a t'Proficiency'. -}
{-# INLINE decode #-}
decode :: Parsers.Binary b s => Parser s InputError Proficiency
decode = fmap (review proficiency) Parsers.word8

{- | Encode a t'Proficiency'. -}
{-# INLINE encode #-}
encode :: Serializers.Binary b s => Serializer s Proficiency
encode = contramap (view proficiency) Serializers.word8
