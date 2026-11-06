{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module: Eeep.Types.Spell.Ability

The spell t'Ability' type.
-}

module Eeep.Types.Spell.Ability (
    -- * Types.
    Ability (..),
) where

-- Imports.
-- Base.
import GHC.Generics (Generic)
import Data.Word (Word8)

-- Libraries.
import Data.ByteString.Short (ShortByteString)

-- Package.
import Eeep.Types.Shared.Projectile (Projectile)
import Eeep.Types.Shared.Resref (Resref)
import Eeep.Types.Spell.Ability.Location (Location)
import Eeep.Types.Spell.Ability.Target (Target)
import Eeep.Types.Spell.Ability.Level (Level)
import Eeep.Types.Spell.Ability.Time (Time)
import Eeep.Types.Spell.Ability.Range (Range)


{- | The t'Ability' @Error@ type.

note(s):

    * To be imported qualified.
-}
data Error
    = LocationError     {-# UNPACK #-} !Word8
    | TargetError       {-# UNPACK #-} !Word8
    | LevelError        {-# UNPACK #-} !Word8
    | TimeError         {-# UNPACK #-} !Word8
    | IconError                        !ShortByteString
    deriving stock (Eq, Ord, Generic, Show)


{- | The spell t'Ability' type. -}
data Ability = Ability {
    location    :: {-# UNPACK #-} !Location,
    target      :: {-# UNPACK #-} !Target,
    range       :: {-# UNPACK #-} !Range,
    level       :: {-# UNPACK #-} !Level,
    time        :: {-# UNPACK #-} !Time,
    icon        :: {-# UNPACK #-} !Resref,
    projectile  :: {-# UNPACK #-} !Projectile
} deriving stock (Eq, Show, Generic)
