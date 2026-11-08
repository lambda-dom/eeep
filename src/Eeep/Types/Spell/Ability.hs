{-# LANGUAGE NoFieldSelectors #-}

{- |
Module: Eeep.Types.Spell.Ability

The spell @Ability@ type.
-}

module Eeep.Types.Spell.Ability (
    -- * Types.
    Ability (..),

    -- ** Getters.
    opCount,
) where

-- Imports.
-- Package.
import GHC.Generics (Generic)

-- Libraries.
import Data.Vector.Strict (Vector)
import Optics.Core (view)

-- Package.
import Eeep.Types.Opcode (Opcode)
import Eeep.Types.Spell.Ability.Header (Header)


{- | The spell @Ability@ type. -}
data Ability = Ability {
    header    :: !Header,
    opcodes   :: !(Vector Opcode)
} deriving stock (Eq, Show, Generic)


{- | Return the number of opcodes in a spell ability. -}
{-# INLINE opCount #-}
opCount :: forall a . Integral a => Ability -> a
opCount = fromIntegral @Int @a . length . view #opcodes
