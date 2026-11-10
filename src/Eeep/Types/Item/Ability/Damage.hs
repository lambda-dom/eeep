{- |
Module: Eeep.Types.Item.Ability.Damage

The @Damage@ type.
-}

module Eeep.Types.Item.Ability.Damage (
    -- * Error types.
    Error (..),

    -- * Types.
    Damage (..),

    -- ** Prisms.
    damage,

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

-- Non-hackage libraries.
import Data.Enum.Optics (enum)
import Trisagion.Utils.Either ((:+:), annotate)
import qualified Trisagion.Typeclasses.Parsers.Binary as Parsers (Binary (..))
import Trisagion.Parser (Parser)
import Trisagion.Parsers.Combinators (validate)
import Trisagion.Parsers.Source (InputError)
import qualified Trisagion.Typeclasses.Serializers.Binary as Serializers (Binary (..))
import Trisagion.Serializer (Serializer)


{- | The t'Damage' @Error@ type. -}
newtype Error = Error Word8
    deriving stock (Eq, Ord, Bounded, Show)
    deriving newtype Enum


{- | The @Damage@ enumeration type for the damage type of weapon abilities. -}
data Damage
    = None
    | Piercing
    | Crushing
    | Slashing
    | Missile
    | Fists
    | PiercingCrushing
    | PiercingSlashing
    | SlashingMissile
    deriving stock (Eq, Ord, Enum, Bounded, Ix, Show)


{- | Prism for the @t'Damage'@ type. -}
{-# INLINE damage #-}
damage :: Prism' Word8 Damage
damage = prism' construct match
    where
        construct :: Damage -> Word8
        construct = succ . fromIntegral @Int @Word8 . review enum

        match :: Word8 -> Maybe Damage
        match = preview enum . pred . fromIntegral @Word8 @Int


{- | Decode a t'Damage'. -}
{-# INLINE decode #-}
decode :: Parsers.Binary b s => Parser s (Error :+: InputError) Damage
decode = validate (annotate Error (preview damage)) Parsers.word8

{- | Encode a t'Damage'. -}
{-# INLINE encode #-}
encode :: Serializers.Binary b s => Serializer s Damage
encode = contramap (review damage) Serializers.word8
