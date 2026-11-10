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

    -- ** Getters.
    abCount,
    opCount,
) where

-- Imports.
-- Base.
import GHC.Generics (Generic)
import Data.Word (Word8)

-- Libraries.
import Data.Vector.Strict (Vector)
import Optics.Core (view)

-- Package.
import Eeep.Types.Opcode (Opcode)
import qualified Eeep.Types.Opcode as Opcode (Error)
import Eeep.Types.Item.Ability (Ability (..))
import qualified Eeep.Types.Item.Header as ItemHeader (Header, Error)
import qualified Eeep.Types.Item.Ability.Header as AbilityHeader (Error)


{- | The t'Spell' @Error@ type. -}
data Error
    = SignatureError !Word8
    | HeaderError    !ItemHeader.Error
    | AbilityError   !AbilityHeader.Error
    | OpcodeError    !Opcode.Error
    deriving stock (Eq, Show, Generic)


{- | The @Item@ type. -}
data Item = Item {
    header    :: !ItemHeader.Header,
    abilities :: !(Vector Ability),
    opcodes   :: !(Vector Opcode)
} deriving stock (Eq, Show, Generic)


{- | Return the number of abilities in a spell. -}
{-# INLINE abCount #-}
abCount :: forall a . Integral a => Item -> a
abCount = fromIntegral @Int @a . length . view #abilities

-- | Return the offset of the abilities table. Not exported.
{-# INLINE abOffset #-}
abOffset :: forall a . Integral a => Item -> a
abOffset = const 0x72

-- | Return the offset of the opcodes table. Not exported.
{-# INLINE opOffset #-}
opOffset :: forall a . Integral a => Item -> a
opOffset spl = abOffset @a spl + (0x28 * abCount @a spl)

{- | Return the number of the global or casting opcodes of the spell. -}
{-# INLINE opCount #-}
opCount :: forall a . Integral a => Item -> a
opCount = fromIntegral @Int @a . length . view #opcodes
