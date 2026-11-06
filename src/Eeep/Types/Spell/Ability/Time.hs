{- |
Module: Eeep.Types.Spell.Ability.Time

The t'Time 'type.
-}

module Eeep.Types.Spell.Ability.Time (
    -- * Error types.
    Error (..),

    -- * Types.
    Time,

    -- ** Prisms.
    time,

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


{- | The t'Time' @Error@ type.

note(s):

    * To be imported qualified.
-}
newtype Error = Error Word8
    deriving stock (Eq, Ord, Bounded, Show, Ix)
    deriving newtype Enum


{- | Wrapper around 'Word8' to constrain the values of the casting time field. -}
newtype Time = Time Word8
    deriving stock (Eq, Ord, Ix)
    deriving newtype (Enum, Show)


-- Instances.
instance Bounded Time where
    {-# INLINE minBound #-}
    minBound :: Time
    minBound = Time 0

    {-# INLINE maxBound #-}
    maxBound :: Time
    maxBound = Time 10


{- | Prism for the @t'Time'@ type. -}
{-# INLINE time #-}
time :: Prism' Word8 Time
time = prism' construct match
    where
        construct :: Time -> Word8
        construct = fromIntegral @Int @Word8 . review enum

        match :: Word8 -> Maybe Time
        match = preview enum . fromIntegral @Word8 @Int

{- | Decode a t'Time'. -}
{-# INLINE decode #-}
decode :: Parsers.Binary b s => Parser s (Error :+: InputError) Time
decode = validate (annotate Error (preview time)) Parsers.word8

{- | Encode a t'Time'. -}
{-# INLINE encode #-}
encode :: Serializers.Binary b s => Serializer s Time
encode = contramap (review time) Serializers.word8
