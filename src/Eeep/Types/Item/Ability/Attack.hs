{- |
Module: Eeep.Types.Item.Ability.Attack

The @Attack@ type.
-}

module Eeep.Types.Item.Ability.Attack (
    -- * Error types.
    Error (..),

    -- * Types.
    Attack (..),

    -- ** Prisms.
    attack,

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


{- | The t'Attack' @Error@ type. -}
newtype Error = Error Word8
    deriving stock (Eq, Ord, Bounded, Show)
    deriving newtype Enum


{- | The @Attack@ enumeration type for the type of attack of item abilities.

note(s):

    * As per the IESDP, None crashes the game for item abilities.
-}
data Attack
    = Melee
    | Ranged
    | Magical
    | Launcher
    deriving stock (Eq, Ord, Enum, Bounded, Ix, Show)


{- | Prism for the @t'Attack'@ type. -}
{-# INLINE attack #-}
attack :: Prism' Word8 Attack
attack = prism' construct match
    where
        construct :: Attack -> Word8
        construct = succ . fromIntegral @Int @Word8 . review enum

        match :: Word8 -> Maybe Attack
        match = preview enum . pred . fromIntegral @Word8 @Int


{- | Decode a t'Attack'. -}
{-# INLINE decode #-}
decode :: Parsers.Binary b s => Parser s (Error :+: InputError) Attack
decode = validate (annotate Error (preview attack)) Parsers.word8

{- | Encode a t'Attack'. -}
{-# INLINE encode #-}
encode :: Serializers.Binary b s => Serializer s Attack
encode = contramap (review attack) Serializers.word8
