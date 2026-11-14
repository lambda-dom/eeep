{- |
Module: Eeep.Types.Item.Category

The @Category@ type.
-}

module Eeep.Types.Item.Category (
    -- * Types.
    Category (..),

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
import Data.Word (Word16)

-- Libraries.
import Optics.Core (Iso', coercedTo, view, review)

-- non-Hackage libraries.
import Trisagion.Parser (Parser)
import Trisagion.Parsers.IStream (InputError)
import qualified Trisagion.Parsers.Binary as Parsers (Binary (..))
import Trisagion.Serializer (Serializer)
import qualified Trisagion.Serializers.Binary as Serializers (Binary (..))


{- | The t'Category' numeric id type of an item.

note(s):

    * t'Category' values can only be validated in the 'IO' monad.
-}
newtype Category = Category Word16
    deriving stock (Eq, Ord, Bounded, Ix)
    deriving newtype (Enum, Show)


{- | Isomorphism for the t'Category' type. -}
{-# INLINE school #-}
school :: Iso' Category Word16
school = coercedTo


{- | Decode a t'Category'. -}
{-# INLINE decode #-}
decode :: Parsers.Binary b s => Parser s InputError Category
decode = fmap (review school) Parsers.word16Le

{- | Encode a t'Category'. -}
{-# INLINE encode #-}
encode :: Serializers.Binary s => Serializer s Category
encode = contramap (view school) Serializers.word16Le
