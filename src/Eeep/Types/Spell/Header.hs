{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module: Eeep.Spell

The Spell 'Header' type.
-}

module Eeep.Types.Spell.Header (
    -- * Types.
    Header (..),
) where

-- Imports.
-- Base.
import GHC.Generics (Generic)
import Data.Word (Word32)

-- Package.
import Eeep.Types.Opcode.Resref (Resref)
import Eeep.Types.Effect.School (School)
import Eeep.Types.Effect.Sectype (Sectype)
import Eeep.Types.Spell.Strref (Strref)
import Eeep.Types.Spell.SpType (SpType)
import Eeep.Types.Spell.Exclusion (Exclusion)
import Eeep.Types.Spell.Flags (Flags)
import Eeep.Types.Spell.Graphics (Graphics)


{- | The Spell @Header@ type. -}
data Header = Header {
    name        :: {-# UNPACK #-} !Strref,
    sound       :: {-# UNPACK #-} !Resref,
    flags       :: {-# UNPACK #-} !Flags,
    sptype      :: {-# UNPACK #-} !SpType,
    exclusion   :: {-# UNPACK #-} !Exclusion,
    graphics    :: {-# UNPACK #-} !Graphics,
    school      :: {-# UNPACK #-} !School,
    sectype     :: {-# UNPACK #-} !Sectype,
    level       :: {-# UNPACK #-} !Word32,
    bookicon    :: {-# UNPACK #-} !Resref,
    description :: {-# UNPACK #-} !Strref
} deriving stock (Eq, Show, Generic)
