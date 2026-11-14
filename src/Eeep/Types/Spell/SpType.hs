{- |
Module: Eeep.Types.Spell.SpType

The @SpType@ type.
-}

module Eeep.Types.Spell.SpType (
    -- * Error types.
    Error (..),

    -- * Types.
    SpType (..),

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


{- | The t'SpType' @Error@ type.

note(s):

    * To be imported qualified.
-}
newtype Error = Error Word8
    deriving stock (Eq, Ord, Bounded, Ix, Show)
    deriving newtype Enum


{- | The spell 'SpType' enumeration type. -}
data SpType
    = Special
    | Wizard
    | Priest
    | Psionic
    | Innate
    | Song
    deriving stock (Eq, Ord, Enum, Bounded, Ix, Show)


{- | Prism for the Spell type @t'SpType'@. -}
{-# INLINE spType #-}
spType :: Prism' Word8 SpType
spType = prism' construct match
    where
        construct :: SpType -> Word8
        construct = fromIntegral @Int @Word8 . review enum

        match :: Word8 -> Maybe SpType
        match = preview enum . fromIntegral @Word8 @Int


{- | Decode a t'SpType'. -}
{-# INLINE decode #-}
decode :: Parsers.Binary b s => Parser s (InputError :+: Error) SpType
decode = validate (validator Error (preview spType)) Parsers.word8

{- | Encode a t'SpType'. -}
{-# INLINE encode #-}
encode :: Serializers.Binary s => Serializer s SpType
encode = contramap (review spType) Serializers.word8
