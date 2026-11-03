{- |
Module: Eeep.Types.Opcode.ResistDispel

The @ResistDispel@ type.
-}

module Eeep.Types.Opcode.ResistDispel (
    -- * Error types.
    Error (..),

    -- * Types.
    ResistDispel (..),

    -- ** Prisms.
    resistDispel,

    -- ** Predicates.
    isDispellable,
    isResistable,

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


{- | The t'ResistDispel' @Error@ type. -}
newtype Error = Error Word8
    deriving stock (Eq, Ord, Bounded, Show)


{- | The @ResistDispel@ enumeration type. -}
data ResistDispel
    = Natural
    | DispellableResistable
    | UndispellableUnresistable
    | DispellableUnresistable
    deriving stock (Eq, Ord, Enum, Bounded, Ix, Show)


{- | Prism for the @t'ResistDispel'@ type. -}
{-# INLINE resistDispel #-}
resistDispel :: Prism' Word8 ResistDispel
resistDispel = prism' construct match
    where
        construct :: ResistDispel -> Word8
        construct = fromIntegral @Int @Word8 . review enum

        match :: Word8 -> Maybe ResistDispel
        match = preview enum . fromIntegral @Word8 @Int


{- | Return True if t'ResistDispel' is dispellable. -}
{-# INLINE isDispellable #-}
isDispellable :: ResistDispel -> Bool
isDispellable = \case
    Natural                   -> False
    DispellableResistable     -> True
    UndispellableUnresistable -> False
    DispellableUnresistable   -> True

{- | Return True if t'ResistDispel' is resistable. -}
{-# INLINE isResistable #-}
isResistable :: ResistDispel -> Bool
isResistable = \case
    Natural                   -> False
    DispellableResistable     -> True
    UndispellableUnresistable -> False
    DispellableUnresistable   -> False


{- | Decode a t'ResistDispel'. -}
{-# INLINE decode #-}
decode :: Parsers.Binary b s => Parser s (Error :+: InputError) ResistDispel
decode = validate (annotate Error (preview resistDispel)) Parsers.word8

{- | Encode a t'ResistDispel'. -}
{-# INLINE encode #-}
encode :: Serializers.Binary b s => Serializer s ResistDispel
encode = contramap (review resistDispel) Serializers.word8
