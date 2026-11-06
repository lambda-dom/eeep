{- |
Module: Eeep.Core.Spell.Graphics

The casting graphics of a spell.
-}

module Eeep.Types.Spell.Graphics (
    -- * Types.
    Graphics (..),

    -- ** Prisms.
    graphics,

    -- ** Parsers and serializers.
    decode,
    encode,
) where

-- Base imports.
import Data.Functor.Contravariant (Contravariant (..))
import Data.Ix (Ix)
import Data.Word (Word16)

-- Libraries.
import Optics.Core (Iso', coercedTo, view, review)

-- non-Hackage libraries.
import qualified Trisagion.Typeclasses.Parsers.Binary as Parsers (Binary (..))
import Trisagion.Parser (Parser)
import Trisagion.Parsers.Source (InputError)
import qualified Trisagion.Typeclasses.Serializers.Binary as Serializers (Binary (..))
import Trisagion.Serializer (Serializer)


{- | The spell's casting graphics encoded as a 'Word16'.

t'Graphics' values can only be validated in the IO monad.
-}
newtype Graphics = Graphics Word16
    deriving stock (Eq, Ord, Bounded, Ix)
    deriving newtype (Enum, Show)


{- | Isomorphism for the t'Graphics' type. -}
{-# INLINE graphics #-}
graphics :: Iso' Graphics Word16
graphics = coercedTo


{- | Decode a t'Graphics'. -}
{-# INLINE decode #-}
decode :: Parsers.Binary b s => Parser s InputError Graphics
decode = fmap (review graphics) Parsers.word16Le

{- | Encode a t'Graphics'. -}
{-# INLINE encode #-}
encode :: Serializers.Binary b s => Serializer s Graphics
encode = contramap (view graphics) Serializers.word16Le
