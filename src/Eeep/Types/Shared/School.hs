{- |
Module: Eeep.Types.Shared.School

The @School@ type.
-}

module Eeep.Types.Shared.School (
    -- * Types.
    School (..),

    -- ** Isomorphisms.
    school,

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
import Trisagion.Parser (Parser)
import Trisagion.Parsers.IStream (InputError)
import qualified Trisagion.Parsers.Binary as Parsers (Binary (..))
import Trisagion.Serializer (Serializer)
import qualified Trisagion.Serializers.Binary as Serializers (Binary (..))


{- | The t'School' numeric id type for the spell magical schools.

note(s):

    * t'School' values can only be validated in the 'IO' monad.
-}
newtype School = School Word8
    deriving stock (Eq, Ord, Bounded, Ix)
    deriving newtype (Enum, Show)


{- | Isomorphism for the t'School' type. -}
{-# INLINE school #-}
school :: Iso' School Word8
school = coercedTo


{- | Decode a t'School'. -}
{-# INLINE decode #-}
decode :: Parsers.Binary b s => Parser s InputError School
decode = fmap (review school) Parsers.word8

{- | Encode a t'School'. -}
{-# INLINE encode #-}
encode :: Serializers.Binary s => Serializer s School
encode = contramap (view school) Serializers.word8
