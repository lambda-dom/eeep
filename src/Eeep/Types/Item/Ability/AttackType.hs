{- |
Module: Eeep.Types.Item.Ability.AttackType

The @AttackType@ type.
-}

module Eeep.Types.Item.Ability.AttackType (
    -- * Error types.
    Error (..),

    -- * Types.
    AttackType (..),

    -- ** Prisms.
    attackType,

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


{- | The t'AttackType' @Error@ type. -}
newtype Error = Error Word8
    deriving stock (Eq, Ord, Bounded, Show)
    deriving newtype Enum


{- | The @AttackType@ enumeration type for the type of attack of item abilities.

note(s):

    * As per the IESDP, None crashes the game for item abilities.
-}
data AttackType
    = Melee
    | Ranged
    | Magical
    | Launcher
    deriving stock (Eq, Ord, Enum, Bounded, Ix, Show)


{- | Prism for @t'AttackType'@. -}
{-# INLINE attackType #-}
attackType :: Prism' Word8 AttackType
attackType = prism' construct match
    where
        construct :: AttackType -> Word8
        construct = succ . fromIntegral @Int @Word8 . review enum

        match :: Word8 -> Maybe AttackType
        match = preview enum . pred . review word8ToInt


{- | Decode a t'AttackType'. -}
{-# INLINE decode #-}
decode :: Parsers.Binary b s => Parser s (InputError :+: Error) AttackType
decode = validate (validator Error (preview attackType)) Parsers.word8

{- | Encode a t'AttackType'. -}
{-# INLINE encode #-}
encode :: Serializers.Binary s => Serializer s AttackType
encode = contramap (review attackType) Serializers.word8
