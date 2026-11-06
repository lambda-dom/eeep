{- |
Module: Eeep.Types.Spell.Level

The Spell @Level@ type.
-}

module Eeep.Types.Spell.Level (
    -- * Error types.
    Error (..),

    -- * Types.
    Level,

    -- ** Prisms.
    level,

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
import Optics.Core (Prism', review, preview, prism')

-- non-Hackage libraries.
import Data.Enum.Optics (enum)
import Trisagion.Utils.Either ((:+:), annotate)
import qualified Trisagion.Typeclasses.Parsers.Binary as Parsers (Binary (..))
import Trisagion.Parser (Parser)
import Trisagion.Parsers.Source (InputError)
import Trisagion.Parsers.Combinators (validate)
import qualified Trisagion.Typeclasses.Serializers.Binary as Serializers (Binary (..))
import Trisagion.Serializer (Serializer)


{- | The t'Level' @Error@ type.

note(s):

    * To be imported qualified.
-}
newtype Error = Error Word8
    deriving stock (Eq, Ord, Bounded, Show, Ix)
    deriving newtype Enum


{- | The @Level@ type. 

Refinement type to constrain the values of spell level to the interval @[0 .. 10]@.
-}
newtype Level = Level Word8
    deriving stock (Eq, Ord, Ix)
    deriving newtype (Enum, Show)


-- Instances.
instance Bounded Level where
    {-# INLINE minBound #-}
    minBound :: Level
    minBound = Level 0

    {-# INLINE maxBound #-}
    maxBound :: Level
    maxBound = Level 10


{- | Prism for the @t'Level'@ type. -}
{-# INLINABLE level #-}
level :: Prism' Word8 Level
level = prism' construct match
    where
        construct :: Level -> Word8
        construct = fromIntegral @Int @Word8 . review enum

        match :: Word8 -> Maybe Level
        match = preview enum . fromIntegral @Word8 @Int


{- | Decode a t'Level'. -}
{-# INLINE decode #-}
decode :: Parsers.Binary b s => Parser s (Error :+: InputError) Level
decode = validate (annotate Error (preview level)) Parsers.word8

{- | Encode a t'Level'. -}
{-# INLINE encode #-}
encode :: Serializers.Binary b s => Serializer s Level
encode = contramap (review level) Serializers.word8
