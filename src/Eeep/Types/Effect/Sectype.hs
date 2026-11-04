{- |
Module: Eeep.Types.Effect.Sectype

The @Sectype@ type.
-}

module Eeep.Types.Effect.Sectype (
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
import Data.Word (Word32)

-- Libraries.
import Optics.Core (Iso', coercedTo, view, review)

-- non-Hackage libraries.
import qualified Trisagion.Typeclasses.Parsers.Binary as Parsers (Binary (..))
import Trisagion.Parser (Parser)
import Trisagion.Parsers.Source (InputError)
import qualified Trisagion.Typeclasses.Serializers.Binary as Serializers (Binary (..))
import Trisagion.Serializer (Serializer)


{- | The t'Sectype' numeric id type.

note(s):

    * t'Sectype' values can only be validated in the 'IO' monad.
-}
newtype Sectype = Sectype Word32
    deriving stock (Eq, Ord, Bounded, Ix, Show)
    deriving newtype Enum


{- | Isomorphism for the t'Sectype' type. -}
{-# INLINE sectype #-}
sectype :: Iso' Sectype Word32
sectype = coercedTo


{- | Decode a t'Sectype'. -}
{-# INLINE decode #-}
decode :: Parsers.Binary b s => Parser s InputError Sectype
decode = fmap (review sectype) Parsers.word32Le

{- | Encode a t'Sectype'. -}
{-# INLINE encode #-}
encode :: Serializers.Binary b s => Serializer s Sectype
encode = contramap (view sectype) Serializers.word32Le
