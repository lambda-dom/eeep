{- |
Module: Eeep.Types.Item.Ability.AbilityType

The @AbilityType@ type.
-}

module Eeep.Types.Item.Ability.AbilityType (
    -- * Error types.
    Error (..),

    -- * Types.
    AbilityType (..),

    -- ** Prisms.
    abilityType,

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
import Data.Word.Optics (word8ToInt)
import Trisagion.Utils.Either ((:+:), validator)
import Trisagion.Parser (Parser)
import Trisagion.Parsers.Combinators (validate)
import Trisagion.Parsers.IStream (InputError)
import qualified Trisagion.Parsers.Binary as Parsers (Binary (..))
import Trisagion.Serializer (Serializer)
import qualified Trisagion.Serializers.Binary as Serializers (Binary (..))


{- | The t'AbilityType' @Error@ type. -}
newtype Error = Error Word8
    deriving stock (Eq, Ord, Bounded, Show)
    deriving newtype Enum


{- | The @AbilityType@ enumeration type for the type of attack of item abilities.

note(s):

    * As per the IESDP, None crashes the game for item abilities.
-}
data AbilityType
    = Melee
    | Ranged
    | Magical
    | Launcher
    deriving stock (Eq, Ord, Enum, Bounded, Ix, Show)


{- | Prism for @t'AbilityType'@. -}
{-# INLINE abilityType #-}
abilityType :: Prism' Word8 AbilityType
abilityType = prism' construct match
    where
        construct :: AbilityType -> Word8
        construct = succ . fromIntegral @Int @Word8 . review enum

        match :: Word8 -> Maybe AbilityType
        match = preview enum . pred . review word8ToInt


{- | Decode a t'AbilityType'. -}
{-# INLINE decode #-}
decode :: Parsers.Binary s => Parser s (InputError :+: Error) AbilityType
decode = validate (validator Error (preview abilityType)) Parsers.word8

{- | Encode a t'AbilityType'. -}
{-# INLINE encode #-}
encode :: Serializers.Binary s => Serializer s AbilityType
encode = contramap (review abilityType) Serializers.word8
