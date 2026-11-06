{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module: Eeep.Spell

The Spell t'Header' type.
-}

module Eeep.Types.Spell.Header (
    -- * Error types.
    Error (..),

    -- * Types.
    Header (..),
) where

-- Imports.
-- Base.
import GHC.Generics (Generic)
import Data.Word (Word8)

-- non-Hackage libraries.
import Data.ByteString.Short (ShortByteString)

-- Package.
import Eeep.Types.Shared.Resref (Resref)
import Eeep.Types.Shared.School (School)
import Eeep.Types.Shared.Sectype (Sectype)
import Eeep.Types.Shared.Strref (Strref)
import Eeep.Types.Spell.SpType (SpType)
import Eeep.Types.Spell.Level (Level)
import Eeep.Types.Spell.Exclusion (Exclusion)
import Eeep.Types.Spell.Flags (Flags)
import Eeep.Types.Spell.Graphics (Graphics)


{- | The t'Header' @Error@ type.

note(s):

    * To be imported qualified.
-}
data Error b
    = SignatureError                   !b
    | SpTypeError       {-# UNPACK #-} !Word8
    | LevelError        {-# UNPACK #-} !Word8
    | SoundError                       !ShortByteString
    | IconError                        !ShortByteString
    deriving stock (Eq, Ord, Generic, Show)


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
    level       :: {-# UNPACK #-} !Level,
    icon        :: {-# UNPACK #-} !Resref,
    description :: {-# UNPACK #-} !Strref
} deriving stock (Eq, Show, Generic)
