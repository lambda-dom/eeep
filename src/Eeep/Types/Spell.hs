{-# LANGUAGE NoFieldSelectors #-}

{- |
Module: Eeep.Types.Spell

The @Spell@ type.
-}

module Eeep.Types.Spell (
    -- * Error types.
    -- * Types.
    Spell (..),
) where

-- Imports.
-- Libraries.
import Data.Vector.Strict (Vector)

-- Package.
import Eeep.Types.Opcode (Opcode)
import Eeep.Types.Spell.Header (Header)
import Eeep.Types.Spell.Ability (Ability)


{- | The @Spell@ type. -}
data Spell = Spell {
    header    :: !Header,
    abilities :: !(Vector Ability),
    opcodes   :: !(Vector Opcode)
} deriving stock (Eq, Show)
