{-# LANGUAGE NoFieldSelectors #-}

{- |
Module: Eeep.Types.Item.Ability

The spell @Ability@ type.
-}

module Eeep.Types.Item.Ability (
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
import Eeep.Types.Item.Ability.Header (Header)


{- | The spell @Ability@ type. -}
data Ability = Ability {
    header    :: !Header,
    opcodes   :: !(Vector Opcode)
} deriving stock (Eq, Show, Generic)


{- | Return the number of opcodes in a spell ability as an 'Integral'. -}
{-# INLINE opCount #-}
opCount :: forall a . Integral a => Ability -> a
opCount = fromIntegral @Int @a . length . view #opcodes
