{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module: Eeep.Types.Item.Animation

The @Animation@ type.
-}

module Eeep.Types.Item.Animation (
    -- * Types.
    Animation (..),

    -- ** Isomorphisms.
    animation,

    -- ** Parsers and serializers.
    decode,
    encode,
) where

-- Imports.
-- Base.
import Data.Char (chr, ord)
import Data.Functor.Contravariant (Contravariant (..))
import Data.Word (Word8)

-- Libraries.
import Optics.Core (Iso', (^.), (%), iso, _1, _2)

-- non-Hackage libraries.
import qualified Trisagion.Typeclasses.Parsers.Binary as Parsers (Binary (..))
import Trisagion.Parser (Parser)
import Trisagion.Parsers.Source (InputError)
import qualified Trisagion.Typeclasses.Serializers.Binary as Serializers (Binary (..))
import Trisagion.Serializer (Serializer)


{- | The item t'Animation' type of an item encoded as a pair of 8-bit characters.

note(s):

    * t'Animation' values can only be validated in the 'IO' monad.
-}
data Animation = Animation {
    lower :: {-# UNPACK #-} !Char,
    upper :: {-# UNPACK #-} !Char
} deriving stock (Eq, Ord, Bounded)


-- Instances.
instance Show Animation where
    show :: Animation -> String
    show (Animation c s) = [c, s]


{- | Isomorphism for the t'Animation' type. -}
{-# INLINE animation #-}
animation :: Iso' Animation (Char, Char)
animation = iso from to
    where
        from :: Animation -> (Char, Char)
        from (Animation c s) = (c, s)

        to :: (Char, Char) -> Animation
        to = uncurry Animation


{- | Decode a t'Animation'. -}
{-# INLINE decode #-}
decode :: Parsers.Binary b s => Parser s InputError Animation
decode = do
    lower <- fmap (chr . fromIntegral @Word8 @Int) Parsers.word8
    upper <- fmap (chr . fromIntegral @Word8 @Int) Parsers.word8
    pure $ Animation {..}

{- | Encode a t'Animation'. -}
{-# INLINE encode #-}
encode :: Serializers.Binary b s => Serializer s Animation
encode
    =  contramap (fromIntegral @Int @Word8 . ord . (^. animation % _1)) Serializers.word8
    <> contramap (fromIntegral @Int @Word8 . ord . (^. animation % _2)) Serializers.word8
