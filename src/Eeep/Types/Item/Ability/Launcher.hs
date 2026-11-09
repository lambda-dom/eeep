{- |
Module: Eeep.Types.Item.Ability.Launcher

The @Launcher@ type.
-}

module Eeep.Types.Item.Ability.Launcher (
    -- * Error types.
    Error (..),

    -- * Types.
    Launcher (..),

    -- ** Prisms.
    launcher,

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
import Trisagion.Utils.Either ((:+:), annotate)
import qualified Trisagion.Typeclasses.Parsers.Binary as Parsers (Binary (..))
import Trisagion.Parser (Parser)
import Trisagion.Parsers.Combinators (validate)
import Trisagion.Parsers.Source (InputError)
import qualified Trisagion.Typeclasses.Serializers.Binary as Serializers (Binary (..))
import Trisagion.Serializer (Serializer)


{- | The t'Launcher' @Error@ type. -}
newtype Error = Error Word8
    deriving stock (Eq, Ord, Bounded, Show)
    deriving newtype Enum


{- | The @Launcher@ enumeration type for the type of ranged attack of item abilities. -}
data Launcher
    = None
    | Bow
    | Crossbow
    | Sling
    | Spear
    | Axe
    deriving stock (Eq, Ord, Enum, Bounded, Ix, Show)


{- | Prism for the @t'Launcher'@ type. -}
{-# INLINE launcher #-}
launcher :: Prism' Word8 Launcher
launcher = prism' construct match
    where
        construct :: Launcher -> Word8
        construct None     = 0
        construct Bow      = 1
        construct Crossbow = 2
        construct Sling    = 3
        construct Spear    = 40
        construct Axe      = 100

        match :: Word8 -> Maybe Launcher
        match n
            | n == 0    = Just None
            | n == 1    = Just Bow
            | n == 2    = Just Crossbow
            | n == 3    = Just Sling
            | n == 40   = Just Spear
            | n == 100  = Just Axe
            | otherwise = Nothing


{- | Decode a t'Launcher'. -}
{-# INLINE decode #-}
decode :: Parsers.Binary b s => Parser s (Error :+: InputError) Launcher
decode = validate (annotate Error (preview launcher)) Parsers.word8

{- | Encode a t'Launcher'. -}
{-# INLINE encode #-}
encode :: Serializers.Binary b s => Serializer s Launcher
encode = contramap (review launcher) Serializers.word8
