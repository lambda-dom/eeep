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
import Data.Word (Word32)

-- Libraries.
import Optics.Core (Iso', coercedTo, view, review)

-- non-Hackage libraries.
import qualified Trisagion.Typeclasses.Parsers.Binary as Parsers (Binary (..))
import Trisagion.Parser (Parser)
import Trisagion.Parsers.Source (InputError)
import qualified Trisagion.Typeclasses.Serializers.Binary as Serializers (Binary (..))
import Trisagion.Serializer (Serializer)


{- | The t'School' numeric id type.

note(s):

    * t'School' values can only be validated in the 'IO' monad.
-}
newtype School = School Word32
    deriving stock (Eq, Ord, Bounded, Ix)
    deriving newtype (Enum, Show)


{- | Isomorphism for the t'School' type. -}
{-# INLINE school #-}
school :: Iso' School Word32
school = coercedTo


{- | Decode a t'School'. -}
{-# INLINE decode #-}
decode :: Parsers.Binary b s => Parser s InputError School
decode = fmap (review school) Parsers.word32Le

{- | Encode a t'School'. -}
{-# INLINE encode #-}
encode :: Serializers.Binary b s => Serializer s School
encode = contramap (view school) Serializers.word32Le
