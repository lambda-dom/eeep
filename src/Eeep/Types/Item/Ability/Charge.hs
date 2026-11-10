{- |
Module: Eeep.Types.Item.Ability.Charge

The @Charge@ type.
-}

module Eeep.Types.Item.Ability.Charge (
    -- * Error types.
    Error (..),

    -- * Types.
    Charge (..),

    -- ** Prisms.
    charge,

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


{- | The t'Charge' @Error@ type. -}
newtype Error = Error Word8
    deriving stock (Eq, Ord, Bounded, Show)
    deriving newtype Enum


{- | The @Charge@ enumeration type for the charging behavior of item abilities. -}
data Charge
    = Remains
    | Vanishes
    | Replaced
    | Recharges
    deriving stock (Eq, Ord, Enum, Bounded, Ix, Show)


{- | Prism for the @t'Charge'@ type. -}
{-# INLINE charge #-}
charge :: Prism' Word8 Charge
charge = prism' construct match
    where
        construct :: Charge -> Word8
        construct = succ . fromIntegral @Int @Word8 . review enum

        match :: Word8 -> Maybe Charge
        match = preview enum . pred . fromIntegral @Word8 @Int


{- | Decode a t'Charge'. -}
{-# INLINE decode #-}
decode :: Parsers.Binary b s => Parser s (Error :+: InputError) Charge
decode = validate (annotate Error (preview charge)) Parsers.word8

{- | Encode a t'Charge'. -}
{-# INLINE encode #-}
encode :: Serializers.Binary b s => Serializer s Charge
encode = contramap (review charge) Serializers.word8
