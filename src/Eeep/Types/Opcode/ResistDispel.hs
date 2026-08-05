{- |
Module: Eeep.Types.Opcode.ResistDispel

The @ResistDispel@ type.
-}

module Eeep.Types.Opcode.ResistDispel (
    -- * Types.
    ResistDispel (..),

    -- ** Prisms.
    resistDispel,

    -- ** Predicates.
    isDispellable,
    isResistable,
) where

-- Imports.
-- Base.
import Data.Ix (Ix)
import Data.Word (Word8)

-- Libraries.
import Optics.Core (Prism')

-- Package.
import Eeep.Utils.Enum (enum)


{- | The @ResistDispel@ enumeration type. -}
data ResistDispel
    = Natural
    | DispellableResistable
    | UndispellableUnresistable
    | DispellableUnresistable
    deriving stock (Eq, Ord, Enum, Bounded, Ix, Show)


{- | Smart constructor for the @t'ResistDispel'@ values from 'Word8'. -}
{-# INLINABLE resistDispel #-}
resistDispel :: Prism' Word8 ResistDispel
resistDispel = enum


{- | Return True if t'ResistDispel' is dispellable. -}
{-# INLINABLE isDispellable #-}
isDispellable :: ResistDispel -> Bool
isDispellable = \case
    Natural                   -> False
    DispellableResistable     -> True
    UndispellableUnresistable -> False
    DispellableUnresistable   -> True

{- | Return True if t'ResistDispel' is dispellable. -}
{-# INLINABLE isResistable #-}
isResistable :: ResistDispel -> Bool
isResistable = \case
    Natural                   -> False
    DispellableResistable     -> True
    UndispellableUnresistable -> False
    DispellableUnresistable   -> False
