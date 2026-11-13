{- |
Module: Eeep.Types.Item.Ability.Damage

The @DamageType@ type.
-}

module Eeep.Types.Item.Ability.Damage (
    -- * Error types.
    Error (..),

    -- * Types.
    DamageType (..),

    -- ** Prisms.
    damageType,

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
import Trisagion.Utils.Either ((:+:), validator)
import Trisagion.Parser (Parser)
import Trisagion.Parsers.Combinators (validate)
import Trisagion.Parsers.IStream (InputError)
import qualified Trisagion.Parsers.Binary as Parsers (Binary (..))
import Trisagion.Serializer (Serializer)
import qualified Trisagion.Serializers.Binary as Serializers (Binary (..))


{- | The t'DamageType' @Error@. -}
newtype Error = Error Word8
    deriving stock (Eq, Ord, Bounded, Show)
    deriving newtype Enum


{- | The @DamageType@ enumeration for the damage type of weapon abilities. -}
data DamageType
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


{- | Prism for the @t'DamageType'@. -}
{-# INLINE damageType #-}
damageType :: Prism' Word8 DamageType
damageType = prism' construct match
    where
        construct :: DamageType -> Word8
        construct = succ . fromIntegral @Int @Word8 . review enum

        match :: Word8 -> Maybe DamageType
        match = preview enum . pred . fromIntegral @Word8 @Int


{- | Decode a t'DamageType'. -}
{-# INLINE decode #-}
decode :: Parsers.Binary b s => Parser s (InputError :+: Error) DamageType
decode = validate (validator Error (preview damageType)) Parsers.word8

{- | Encode a t'DamageType'. -}
{-# INLINE encode #-}
encode :: Serializers.Binary s => Serializer s DamageType
encode = contramap (review damageType) Serializers.word8
