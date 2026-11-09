{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module: Eeep.Types.Item.Ability.Header

The item ability @Header@ type.
-}

module Eeep.Types.Item.Ability.Header (
    -- * Error types.
    -- * Types.
    Header (..),
) where

-- Imports.
-- Base.
import GHC.Generics (Generic)
import Data.Int (Int16)
import Data.Word (Word8, Word16)

-- Package.
import Eeep.Types.Shared.Ability.Location (Location)
import Eeep.Types.Shared.Ability.Range (Range)
import Eeep.Types.Shared.Ability.Speed (Speed)
import Eeep.Types.Shared.Ability.Target (Target)
import Eeep.Types.Shared.Dice (Dice)
import Eeep.Types.Shared.Projectile (Projectile)
import Eeep.Types.Shared.Resref (Resref)
import Eeep.Types.Shared.School (School)
import Eeep.Types.Shared.Sectype (Sectype)
import Eeep.Types.Item.Ability.Attack (Attack)
import Eeep.Types.Item.Ability.Charge (Charge)
import Eeep.Types.Item.Ability.Damage (Damage)
import Eeep.Types.Item.Ability.Flags (Flags)
import Eeep.Types.Item.Ability.Launcher (Launcher)
import Eeep.Types.Item.Ability.Melee (Melee)


{- | The item ability @Header@ type. -}
data Header = Header {
    attack      :: {-# UNPACK #-} !Attack,
    idRequired  :: {-# UNPACK #-} !Bool,
    location    :: {-# UNPACK #-} !Location,
    icon        :: {-# UNPACK #-} !Resref,
    target      :: {-# UNPACK #-} !Target,
    count       :: {-# UNPACK #-} !Word8,
    range       :: {-# UNPACK #-} !Range,
    launcher    :: {-# UNPACK #-} !Launcher,
    speed       :: {-# UNPACK #-} !Speed,
    thac0       :: {-# UNPACK #-} !Int16,
    damage      :: {-# UNPACK #-} !Int16,
    alternative :: {-# UNPACK #-} !Dice,
    dice        :: {-# UNPACK #-} !Dice,
    school      :: {-# UNPACK #-} !School,
    sectype     :: {-# UNPACK #-} !Sectype,
    damtype     :: {-# UNPACK #-} !Damage,
    charge      :: {-# UNPACK #-} !Charge,
    charges     :: {-# UNPACK #-} !Word16,
    flags       :: {-# UNPACK #-} !Flags,
    projectile  :: {-# UNPACK #-} !Projectile,
    melee       :: {-# UNPACK #-} !Melee,
    isArrow     :: {-# UNPACK #-} !Bool,
    isBolt      :: {-# UNPACK #-} !Bool,
    isBullet    :: {-# UNPACK #-} !Bool
} deriving stock (Eq, Show, Generic)
