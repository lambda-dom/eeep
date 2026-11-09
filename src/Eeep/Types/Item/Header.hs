{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module: Eeep.Types.Item.Header

The item t'Header' type.
-}

module Eeep.Types.Item.Header (
    -- * Error types.
    -- * Types.
    Header (..),
) where

-- Imports.
-- Base.
import GHC.Generics (Generic)
import Data.Word (Word8, Word16, Word32)

-- Package.
import Eeep.Types.Shared.Attribute (Attribute)
import Eeep.Types.Shared.Resref (Resref)
import Eeep.Types.Shared.Strref (Strref)
import Eeep.Types.Shared.Strength (Strength)
import Eeep.Types.Item.Flags (Flags)
import Eeep.Types.Item.Animation (Animation)
import Eeep.Types.Item.Category (Category)
import Eeep.Types.Item.KitUsability (KitUsability)
import Eeep.Types.Item.Usability (Usability)


{- | The item @Header@ type. -}
data Header = Header {
    name            :: {-# UNPACK #-} !Strref,
    description     :: {-# UNPACK #-} !Strref,
    nameId          :: {-# UNPACK #-} !Strref,
    descriptionId   :: {-# UNPACK #-} !Strref,
    category        :: {-# UNPACK #-} !Category,
    flags           :: {-# UNPACK #-} !Flags,
    usability       :: {-# UNPACK #-} !Usability,
    kitUsability    :: {-# UNPACK #-} !KitUsability,
    price           :: {-# UNPACK #-} !Word32,
    stack           :: {-# UNPACK #-} !Word16,
    lore            :: {-# UNPACK #-} !Word16,
    weight          :: {-# UNPACK #-} !Word32,
    enchantment     :: {-# UNPACK #-} !Word8,
    level           :: {-# UNPACK #-} !Word8,
    strength        :: {-# UNPACK #-} !Strength,
    intelligence    :: {-# UNPACK #-} !Attribute,
    dexterity       :: {-# UNPACK #-} !Attribute,
    wisdom          :: {-# UNPACK #-} !Attribute,
    constitution    :: {-# UNPACK #-} !Attribute,
    charisma        :: {-# UNPACK #-} !Attribute,
    animation       :: {-# UNPACK #-} !Animation,
    replacement     :: {-# UNPACK #-} !Resref,
    inventoryIcon   :: {-# UNPACK #-} !Resref,
    groundIcon      :: {-# UNPACK #-} !Resref,
    descriptionIcon :: {-# UNPACK #-} !Resref
} deriving stock (Eq, Show, Generic)
