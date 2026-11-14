{- |
Module: Eeep.Types.Shared.Ability.Location

The ability @Location@ type.
-}

module Eeep.Types.Shared.Ability.Location (
    -- * Error types.
    Error (..),

    -- * Types.
    Location (..),

    -- ** Prisms.
    location,

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
import Data.Word.Optics (word8ToInt)
import Trisagion.Utils.Either ((:+:), validator)
import Trisagion.Parser (Parser)
import Trisagion.Parsers.Combinators (validate)
import Trisagion.Parsers.IStream (InputError)
import qualified Trisagion.Parsers.Binary as Parsers (Binary (..))
import Trisagion.Serializer (Serializer)
import qualified Trisagion.Serializers.Binary as Serializers (Binary (..))


{- | The t'Location' @Error@ type.

note(s):

    * To be imported qualified.
-}
newtype Error = Error Word8
    deriving stock (Eq, Ord, Bounded, Ix, Show)
    deriving newtype Enum


{- | The 'Location' enumeration type for the ui location of abilities. -}
data Location
    = None
    | Weapon
    | Spell
    | Item
    | Innate
    deriving stock (Eq, Ord, Enum, Bounded, Ix, Show)


{- | Prism for the @t'Location'@ type. -}
{-# INLINE location #-}
location :: Prism' Word8 Location
location = prism' construct match
    where
        construct :: Location -> Word8
        construct = fromIntegral @Int @Word8 . review enum

        match :: Word8 -> Maybe Location
        match = preview enum . review word8ToInt


{- | Decode a t'Location'. -}
{-# INLINE decode #-}
decode :: Parsers.Binary b s => Parser s (InputError :+: Error) Location
decode = validate (validator Error (preview location)) Parsers.word8

{- | Encode a t'Location'. -}
{-# INLINE encode #-}
encode :: Serializers.Binary s => Serializer s Location
encode = contramap (review location) Serializers.word8
