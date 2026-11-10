{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module: Eeep.Types.Item

The @Item@ type.
-}

module Eeep.Types.Item (
    -- * Error types.
    -- * Types.
    Item (..),
) where

-- Imports.
-- Base.
import GHC.Generics (Generic)

-- Libraries.
import Data.Vector.Strict (Vector)

-- Package.
import Eeep.Types.Opcode (Opcode)
import Eeep.Types.Item.Ability (Ability (..))
import qualified Eeep.Types.Item.Header as ItemHeader (Header)


{- | The @Item@ type. -}
data Item = Item {
    header    :: !ItemHeader.Header,
    abilities :: !(Vector Ability),
    opcodes   :: !(Vector Opcode)
} deriving stock (Eq, Show, Generic)

