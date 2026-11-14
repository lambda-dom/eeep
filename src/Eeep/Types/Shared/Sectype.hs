{- |
Module: Eeep.Types.Shared.Sectype

The @Sectype@ type.
-}

module Eeep.Types.Shared.Sectype (
    -- * Types.
    Sectype (..),

    -- ** Isomorphisms.
    sectype,

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
import qualified Trisagion.Parsers.Binary as Parsers (Binary (..))
import Trisagion.Parser (Parser)
import Trisagion.Parsers.IStream (InputError)
import qualified Trisagion.Serializers.Binary as Serializers (Binary (..))
import Trisagion.Serializer (Serializer)


{- | The t'Sectype' numeric id type for (spell) sectypes.

note(s):

    * t'Sectype' values can only be validated in the 'IO' monad.
-}
newtype Sectype = Sectype Word8
    deriving stock (Eq, Ord, Bounded, Ix)
    deriving newtype (Enum, Show)


{- | Isomorphism for the t'Sectype' type. -}
{-# INLINE sectype #-}
sectype :: Iso' Sectype Word8
sectype = coercedTo


{- | Decode a t'Sectype'. -}
{-# INLINE decode #-}
decode :: Parsers.Binary b s => Parser s InputError Sectype
decode = fmap (review sectype) Parsers.word8

{- | Encode a t'Sectype'. -}
{-# INLINE encode #-}
encode :: Serializers.Binary s => Serializer s Sectype
encode = contramap (view sectype) Serializers.word8
