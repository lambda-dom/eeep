{- |
Module: Eeep.Types.Spell.SpellType

The @SpellType@ type.
-}

module Eeep.Types.Spell.SpellType (
    -- * Error types.
    Error (..),

    -- * Types.
    SpellType (..),

    -- ** Prisms.
    spType,

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
import Trisagion.Utils.Either ((:+:), validator)
import Trisagion.Parser (Parser)
import Trisagion.Parsers.Combinators (validate)
import Trisagion.Parsers.IStream (InputError)
import qualified Trisagion.Parsers.Binary as Parsers (Binary (..))
import Trisagion.Serializer (Serializer)
import qualified Trisagion.Serializers.Binary as Serializers (Binary (..))


{- | The t'SpellType' @Error@ type.

note(s):

    * To be imported qualified.
-}
newtype Error = Error Word8
    deriving stock (Eq, Ord, Bounded, Ix, Show)
    deriving newtype Enum


{- | The 'SpellType' enumeration type. -}
data SpellType
    = Special
    | Wizard
    | Priest
    | Psionic
    | Innate
    | Song
    deriving stock (Eq, Ord, Enum, Bounded, Ix, Show)


{- | Prism for @t'SpellType'@. -}
{-# INLINE spType #-}
spType :: Prism' Word8 SpellType
spType = prism' construct match
    where
        construct :: SpellType -> Word8
        construct = fromIntegral @Int @Word8 . review enum

        match :: Word8 -> Maybe SpellType
        match = preview enum . fromIntegral @Word8 @Int


{- | Decode a t'SpellType'. -}
{-# INLINE decode #-}
decode :: Parsers.Binary s => Parser s (InputError :+: Error) SpellType
decode = validate (validator Error (preview spType)) Parsers.word8

{- | Encode a t'SpellType'. -}
{-# INLINE encode #-}
encode :: Serializers.Binary s => Serializer s SpellType
encode = contramap (review spType) Serializers.word8
