{-# LANGUAGE UndecidableInstances #-}

{- |
Module: Eeep.Types.Opcode.ResistDispel

The @ResistDispel@ type.
-}

module Eeep.Types.Opcode.ResistDispel (
    -- * Error types.
    ResistDispelError (..),

    -- * Types.
    ResistDispel (..),

    -- ** Constructors.
    resistDispel,
) where

-- Imports.
-- Base.
import Data.Functor.Contravariant (Contravariant (..))
import Data.Ix (Ix)
import Data.Word (Word8)

-- non-Hackage libraries.
import Trisagion.Utils.Either ((:+:))
import Trisagion.Typeclasses.Source (Source)
import Trisagion.Typeclasses.Sink (Sink)
import Trisagion.Parser (Parser)
import Trisagion.Parsers.Combinators (validate)
import Trisagion.Parsers.Source (InputError, one)
import Trisagion.Serializer (Serializer)
import Trisagion.Serializers.Binary (Binary, word8)

-- Package.
import Eeep.Utils.Enum (eitherEnum)
import Eeep.Typeclasses.Binary (Reader (..), Writer (..))


{- | The t'ResistDispelError' type. -}
newtype ResistDispelError = ResistDispelError Word8
    deriving stock (Eq, Ord, Bounded, Ix, Show)
    deriving newtype Enum


{- | The @ResistDispel@ enumeration type. -}
data ResistDispel
    = Natural
    | DispellableResistable
    | UndispellableUnresistable
    | DispellableUnresistable
    deriving stock (Eq, Ord, Enum, Bounded, Ix, Show)


-- Instances.
instance Source Word8 s => Reader s (ResistDispelError :+: InputError) ResistDispel where
    {-# INLINE parser #-}
    parser :: Parser s (ResistDispelError :+: InputError) ResistDispel
    parser = validate resistDispel one

instance (Sink Word8 b s, Binary b s) => Writer b s ResistDispel where
    {-# INLINE serializer #-}
    serializer :: Serializer s ResistDispel
    serializer = contramap (fromIntegral . fromEnum) word8


{- | Smart constructor for the @t'ResistDispel'@ type. -}
{-# INLINE resistDispel #-}
resistDispel :: Word8 -> ResistDispelError :+: ResistDispel
resistDispel n = eitherEnum (ResistDispelError n) n
