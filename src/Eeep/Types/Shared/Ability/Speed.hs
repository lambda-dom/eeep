{- |
Module: Eeep.Types.Shared.Ability.Speed

The t'Speed 'type.
-}

module Eeep.Types.Shared.Ability.Speed (
    -- * Error types.
    Error (..),

    -- * Types.
    Speed,

    -- ** Prisms.
    speed,

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
import Optics.Core (Prism', prism', review, preview)

-- non-Hackage libraries.
import Data.Enum.Optics (enum)
import Trisagion.Utils.Either ((:+:), annotate)
import qualified Trisagion.Typeclasses.Parsers.Binary as Parsers (Binary (..))
import Trisagion.Parser (Parser)
import Trisagion.Parsers.Source (InputError)
import Trisagion.Parsers.Combinators (validate)
import qualified Trisagion.Typeclasses.Serializers.Binary as Serializers (Binary (..))
import Trisagion.Serializer (Serializer)


{- | The t'Speed' @Error@ type.

note(s):

    * To be imported qualified.
-}
newtype Error = Error Word8
    deriving stock (Eq, Ord, Bounded, Show, Ix)
    deriving newtype Enum


{- | Refinement type to constrain the values of the ability speed to the interval [0 .. 10]. -}
newtype Speed = Speed Word8
    deriving stock (Eq, Ord, Ix)
    deriving newtype (Enum, Show)


-- Instances.
instance Bounded Speed where
    {-# INLINE minBound #-}
    minBound :: Speed
    minBound = Speed 0

    {-# INLINE maxBound #-}
    maxBound :: Speed
    maxBound = Speed 10


{- | Prism for the @t'Speed'@ type. -}
{-# INLINE speed #-}
speed :: Prism' Word8 Speed
speed = prism' construct match
    where
        construct :: Speed -> Word8
        construct = fromIntegral @Int @Word8 . review enum

        match :: Word8 -> Maybe Speed
        match = preview enum . fromIntegral @Word8 @Int

{- | Decode a t'Speed'. -}
{-# INLINE decode #-}
decode :: Parsers.Binary b s => Parser s (Error :+: InputError) Speed
decode = validate (annotate Error (preview speed)) Parsers.word8

{- | Encode a t'Speed'. -}
{-# INLINE encode #-}
encode :: Serializers.Binary b s => Serializer s Speed
encode = contramap (review speed) Serializers.word8
