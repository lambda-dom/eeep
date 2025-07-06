{- |
Module: Eeep.Types.Opcode.ResistDispel

The @ResistDispel@ type.
-}

module Eeep.Types.Opcode.ResistDispel (
    -- * Error types.
    ResistDispelError (..),

    -- * Types.
    ResistDispel,

    -- ** Constructors.
    toResistDispel,
) where

-- Imports.
-- Base.
import Data.Bifunctor (Bifunctor (..))
import Data.Ix (Ix)
import Data.Void (absurd)
import Data.Word (Word8)

-- non-Hackage libraries.
import Mono.Typeclasses.MonoFunctor (ElementOf)
import Trisagion.Types.ParseError (ParseError)
import Trisagion.Typeclasses.HasOffset (HasOffset)
import Trisagion.Parser (Parser)
import Trisagion.Parsers.ParseError (throwParseError, capture)
import Trisagion.Parsers.Word8 (word8)

-- Package.
import Eeep.Typeclasses.Binary (Reader (..))


{- | The t'ResistDispelError' type. -}
newtype ResistDispelError = ResistDispelError Word8
    deriving stock (Eq, Show)


{- | The @ResistDispel@ enumeration type. -}
data ResistDispel
    = Natural
    | DispellableResistable
    | UndispellableUnresistable
    | DispellableUnresistable
    deriving stock (Eq, Ord, Enum, Bounded, Ix, Show)


-- Instances
instance (HasOffset s, ElementOf s ~ Word8) => Reader s ResistDispelError ResistDispel where
    parser :: Parser s (ParseError ResistDispelError) ResistDispel
    parser = capture $ do
        n <- first (fmap absurd) word8
        maybe (throwParseError $ ResistDispelError n) pure (toResistDispel n)


{- | Smart constructor for the 'ResistDispel' type.-}
{-# INLINE toResistDispel #-}
toResistDispel :: Word8 -> Maybe ResistDispel
toResistDispel n = if n <= 3 then Just $ toEnum (fromIntegral n) else Nothing
