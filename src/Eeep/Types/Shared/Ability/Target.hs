{- |
Module: Eeep.Types.Shared.Ability.Target

The t'Target' type for the ability's target field.
-}

module Eeep.Types.Shared.Ability.Target (
    -- * Error types.
    Error (..),

    -- * Types.
    Target (..),

    -- ** Prisms.
    target,

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
import Trisagion.Parsers.Combinators (validate)
import Trisagion.Parsers.Source (InputError)
import qualified Trisagion.Typeclasses.Serializers.Binary as Serializers (Binary (..))
import Trisagion.Serializer (Serializer)


{- | The t'Target' @Error@ type.

note(s):

    * To be imported qualified.
-}
newtype Error = Error Word8
    deriving stock (Eq, Ord, Bounded, Ix, Show)
    deriving newtype Enum


{- | The Target enumeration type of spell abilities. -}
data Target
    = Invalid
    | Actor
    | Inventory
    | Dead
    | Point
    | Caster
    | InstantCaster
    deriving stock (Eq, Ord, Enum, Bounded, Ix, Show)


{- | Prism for the @t'Target'@ type. -}
{-# INLINE target #-}
target :: Prism' Word8 Target
target = prism' construct match
    where
        -- Fix InstantCaster.
        to :: Int -> Int
        to n = if 6 == n then 7 else n

        from :: Int -> Int
        -- Will fail in the 6 case per IESDP.
        from 6 = 7
        from 7 = 6
        from n = n

        construct :: Target -> Word8
        construct = fromIntegral @Int @Word8 . to . review enum

        match :: Word8 -> Maybe Target
        match = preview enum . from . fromIntegral @Word8 @Int


{- | Decode a t'Target'. -}
{-# INLINE decode #-}
decode :: Parsers.Binary b s => Parser s (Error :+: InputError) Target
decode = validate (annotate Error (preview target)) Parsers.word8

{- | Encode a t'Target'. -}
{-# INLINE encode #-}
encode :: Serializers.Binary b s => Serializer s Target
encode = contramap (review target) Serializers.word8
